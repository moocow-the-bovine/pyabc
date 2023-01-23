from __future__ import division
import copy
from fractions import Fraction
import re, sys


str_type = str if sys.version > '3' else basestring


# Just some tunes to test against
tunes = [
"""
X: 1
T: The Road To Lisdoonvarna
R: slide
M: 12/8
L: 1/8
K: Edor
E2B B2A B2c d2A|F2A ABA D2E FED|E2B B2A B2c d3|cdc B2A B2E E3:|
|:e2f gfe d2B Bcd|c2A ABc d2B B3|e2f gfe d2B Bcd|cdc B2A B2E E3:||
""",
"""
X: 6
T: The Kid On The Mountain
R: slip jig
M: 9/8
L: 1/8
K: Emin
~E3 FEF G2 F| ~E3 BcA BGD| ~E3 FEF G2 A| BAG FAG FED:|
BGB AFD G2 D| GAB dge dBA| BGB AFA G2 A| BAG FAG FED:|
~g3 eBe e2 f|~g3 efg afd| ~g3 eBe g2 a|bag fag fed:|
eB/B/B e2f ~g3|eB/B/B efg afd| eB/B/B e2f g2a|bag fag fed:|
edB dBA G2D|GAB dge dBA|edB dBA G2A|BAG FAG FED:|
""",
"""
X:7
T:test tics
M:4/4
L:1/8
K:C
[AB] C2 D2 [EF] | (3abc (5defg^g a'2 b'2 ||
""",
"""
X:8
T:test stretch
M:2/4
L:1/8
K:F
A/B/C/D/ z G/^G/ | Z2 |]
"""
]

# Information field table copied from
# http://abcnotation.com/wiki/abc:standard:v2.1#abc_files_tunes_and_fragments
# Columns are:
# X:Field, file header, tune header, tune body, inline, type
information_field_table = """
A:area              yes     yes     no      no      string
B:book              yes     yes     no      no      string
C:composer          yes     yes     no      no      string
D:discography       yes     yes     no      no      string
F:file url          yes     yes     no      no      string
G:group             yes     yes     no      no      string
H:history           yes     yes     no      no      string
I:instruction       yes     yes     yes     yes     instruction
K:key               no      yes     yes     yes     instruction
L:unit note length  yes     yes     yes     yes     instruction
M:meter             yes     yes     yes     yes     instruction
m:macro             yes     yes     yes     yes     instruction
N:notes             yes     yes     yes     yes     string
O:origin            yes     yes     no      no      string
P:parts             no      yes     yes     yes     instruction
Q:tempo             no      yes     yes     yes     instruction
R:rhythm            yes     yes     yes     yes     string
r:remark            yes     yes     yes     yes     -
S:source            yes     yes     no      no      string
s:symbol line       no      no      yes     no      instruction
T:tune title        no      yes     yes     no      string
U:user defined      yes     yes     yes     yes     instruction
V:voice             no      yes     yes     yes     instruction
W:words             no      yes     yes     no      string
w:words             no      no      yes     no      string
X:reference number  no      yes     no      no      instruction
Z:transcription     yes     yes     no      no      string
"""

##======================================================================
class InfoKey(object):
    def __init__(self, key, name, file_header, tune_header, tune_body, inline, type):
        self.key = key  # single-letter field identifier
        self.name = name.strip()  # information field name
        self.file_header = file_header=='yes'  # may be used in file header
        self.tune_header = tune_header=='yes'  # may be used in tune header
        self.tune_body = tune_body=='yes'  # may be used in tune body
        self.inline = inline=='yes'  # nay be used inline in tunes
        self.type = type.strip()  # data type:  string, instruction, or -

# parse info field table
info_keys = {}
for line in information_field_table.split('\n'):
    if line.strip() == '':
        continue
    key = line[0]
    fields = re.match(r'(.*)\s+(yes|no)\s+(yes|no)\s+(yes|no)\s+(yes|no)\s+(.*)', line[2:]).groups()
    info_keys[key] = InfoKey(key, *fields)

file_header_fields = {k:v for k,v in info_keys.items() if v.file_header}
tune_header_fields = {k:v for k,v in info_keys.items() if v.tune_header}
tune_body_fields = {k:v for k,v in info_keys.items() if v.tune_body}
inline_fields = {k:v for k,v in info_keys.items() if v.inline}



# map natural note letters to chromatic values
pitch_values = {'C': 0, 'D': 2, 'E': 4, 'F': 5, 'G': 7, 'A': 9, 'B': 11, }
accidental_values = {'': 0, '#': 1, 'b': -1}
for n,v in list(pitch_values.items()):
    for a in '#b':
        pitch_values[n+a] = v + accidental_values[a]

# map chromatic number back to most common key names
chromatic_notes = ['C', 'C#', 'D', 'Eb', 'E', 'F', 'F#', 'G', 'Ab', 'A', 'Bb', 'B']

# map mode names relative to Ionian (in chromatic steps)
mode_values = {'major': 0, 'minor': 3, 'ionian': 0, 'aeolian': 3,
               'mixolydian': -7, 'dorian': -2, 'phrygian': -4, 'lydian': -5,
               'locrian': 1}

# mode name normalization
mode_abbrev = {m[:3]: m for m in mode_values}

# sharps/flats in ionian keys
key_sig = {'C#': 7, 'F#': 6, 'B': 5, 'E': 4, 'A': 3, 'D': 2, 'G': 1, 'C': 0,
           'F': -1, 'Bb': -2, 'Eb': -3, 'Ab': -4, 'Db': -5, 'Gb': -6, 'Cb': -7}
sharp_order = "FCGDAEB"
flat_order = "BEADGCF"

##======================================================================
class Key(object):
    def __init__(self, name=None, root=None, mode=None):
        if name is not None:
            self.root, self.mode = self.parse_key(name)
            assert root is None and mode is None
        else:
            self.root = Pitch(root)
            self.mode = mode

    def to_json(self):
        root = self.root.name if self.root is not None else None
        return [root, self.mode]

    def parse_key(self, key):
        # highland pipe keys
        if key in ['HP', 'Hp']:
            return {'F': 1, 'C': 1, 'G': 0}

        m = re.match(r'([A-G])(\#|b)?\s*(\w+)?(.*)', key)
        if m is None:
            raise ValueError('Invalid key "%s"' % key)
        base, acc, mode, extra = m.groups()
        if acc is None:
            acc = ''
        if mode is None:
            mode = 'major'
        if mode == 'm':
            mode = 'minor'
        try:
            mode = mode_abbrev[mode[:3].lower()]
        except KeyError:
            raise ValueError("Unrecognized key signature %s" % key)

        return Pitch(base+acc), mode

    @property
    def key_signature(self):
        """
        List of accidentals that should be displayed in the key
        signature for the given key description.
        """
        # determine number of sharps/flats for this key by first converting
        # to ionian, then doing the key lookup
        key = self.relative_ionian
        num_acc = key_sig[key.root.name]

        sig = []
        # sharps or flats?
        if num_acc > 0:
            for i in range(num_acc):
                sig.append(sharp_order[i] + '#')
        else:
            for i in range(-num_acc):
                sig.append(flat_order[i] + 'b')

        return sig

    @property
    def accidentals(self):
        """A dictionary of accidentals in the key signature.
        """
        return {p:a for p,a in self.key_signature}

    @property
    def relative_ionian(self):
        """
        Return the ionian mode relative to the given key and mode.
        """
        key, mode = self.root, self.mode
        rel = mode_values[mode]
        root = Pitch((key.value + rel) % 12)

        # Select flat or sharp to match the current key name
        if '#' in key.name:
            root2 = root.equivalent_sharp
            if len(root2.name) == 2:
                root = root2
        elif 'b' in key.name:
            root2 = root.equivalent_flat
            if len(root2.name) == 2:
                root = root2

        return Key(root=root, mode='ionian')

    def __repr__(self):
        return "<Key %s %s>" % (self.root.name, self.mode)

    def pitch_str(self, pitch):
        """Get abc pitch string with respect to this key"""
        p = pitch.name[0] if pitch.name in self.key_signature else pitch.name

##======================================================================
class Pitch(object):
    def __init__(self, value, octave=None):
        if isinstance(value, Note):
            self._note = value

            if len(value.note) == 1:
                acc = value.key.accidentals.get(value.note[0].upper(), '')
                self._name = value.note.upper() + acc
                self._value = self.pitch_value(self._name)
            else:
                self._name = value.note.capitalize()
                self._value = self.pitch_value(value.note)

            assert octave is None
            self._octave = value.octave
        elif isinstance(value, str_type):
            self._name = value
            self._value = self.pitch_value(value)
            self._octave = octave
        elif isinstance(value, Pitch):
            self._name = value._name
            self._value = value._value
            self._octave = value._octave
        else:
            self._name = None
            if octave is None:
                self._value = value
                self._octave = octave
            else:
                self._value = value % 12
                self._octave = octave + (value // 12)

    def __repr__(self):
        return "<Pitch %s>" % self.name

    @property
    def name(self):
        if self._name is not None:
            return self._name
        return chromatic_notes[self.value%12]

    @property
    def value(self):
        return self._value

    @property
    def octave(self):
        return self._octave

    @property
    def abs_value(self):
        return self.value + self.octave * 12

    def abc_pitch(self, key=None, accidentals=False):
        """Return abc pitch string relative to key, including accidentals."""
        abc = ''
        if accidentals and (key is None or self.name not in key.key_signature):
            abc = self.name[1:].replace('b','_').replace('#','^')
        abc += self.name[0]
        octave = self.octave or 0
        if octave > 0:
            abc = abc.lower()
            abc += "'" * (octave - 1)
        else:
            abc += "," * (-octave)
        return abc

    def to_json(self):
        return {
            'name': self.name,
            'value': self.value,
            'octave': self.octave,
            'abs_value': self.abs_value,
        }

    @staticmethod
    def pitch_value(pitch, root='C'):
        """Convert a pitch string like "A#" to a chromatic scale value relative
        to root.
        """
        pitch = pitch.strip()
        val = pitch_values[pitch[0].upper()]
        for acc in pitch[1:]:
            val += accidental_values[acc]
        if root == 'C':
            return val
        return (val - Pitch.pitch_value(root)) % 12

    def __eq__(self, a):
        return self.value == a.value

    @property
    def equivalent_sharp(self):
        p = self - 1
        if len(p.name) == 1:
            return Pitch(p.name + '#', octave=self.octave)
        else:
            return Pitch((self-2).name + '##', octave=self.octave)

    @property
    def equivalent_flat(self):
        p = self + 1
        if len(p.name) == 1:
            return Pitch(p.name + 'b', octave=self.octave)
        else:
            return Pitch((self+2).name + 'bb', octave=self.octave)

    def __add__(self, x):
        return Pitch(self.value+x, octave=self.octave)

    def __sub__(self, x):
        return Pitch(self.value-x, octave=self.octave)

##======================================================================
class TimeSignature(object):
    def __init__(self, meter, unit_len, tempo=None):
        meter = meter.replace('C|', '2/2').replace('C', '4/4').replace('1/1', '4/4')
        self._meter = [int(x) for x in meter.split('/')]
        self._unit_len = [int(x) for x in unit_len.split('/')]
        self._tempo = tempo

    def __repr__(self):
        return "<TimeSignature %d/%d>" % tuple(self._meter)

    def __str__(self):
        return '\n'.join(filter(bool, [
            f'M:{self._meter[0]}/{self._meter[1]}',
            f'L:{self._unit_len[0]}/{self._unit_len[1]}',
            (f'T:{self_tempo}' if self._tempo else None),
        ]))

    @property
    def meter_length(self):
        return self._meter[0] / self._meter[1]

    @property
    def unit_length(self):
        return self._unit_len[0] / self._unit_len[1]

    @property
    def bar_length(self):
        """Return length of a bar in elementary units"""
        return self.meter_length / self.unit_length

    def stretch_meter(self, frac):
        """Stretch meter to accommodate multiplying all note lengths by ``frac``"""
        prev = Fraction(*self._meter)
        self._meter[0] *= frac.numerator
        self._meter[1] *= frac.denominator
        if self._tempo:
            self._tempo = int(frac * self._tempo)

# Decoration symbols from
# http://abcnotation.com/wiki/abc:standard:v2.1#decorations
symbols = """
!trill!                "tr" (trill mark)
!trill(!               start of an extended trill
!trill)!               end of an extended trill
!lowermordent!         short /|/|/ squiggle with a vertical line through it
!uppermordent!         short /|/|/ squiggle
!mordent!              same as !lowermordent!
!pralltriller!         same as !uppermordent!
!roll!                 a roll mark (arc) as used in Irish music
!turn!                 a turn mark (also known as gruppetto)
!turnx!                a turn mark with a line through it
!invertedturn!         an inverted turn mark
!invertedturnx!        an inverted turn mark with a line through it
!arpeggio!             vertical squiggle
!>!                    > mark
!accent!               same as !>!
!emphasis!             same as !>!
!fermata!              fermata or hold (arc above dot)
!invertedfermata!      upside down fermata
!tenuto!               horizontal line to indicate holding note for full duration
!0! - !5!              fingerings
!+!                    left-hand pizzicato, or rasp for French horns
!plus!                 same as !+!
!snap!                 snap-pizzicato mark, visually similar to !thumb!
!slide!                slide up to a note, visually similar to a half slur
!wedge!                small filled-in wedge mark
!upbow!                V mark
!downbow!              squared n mark
!open!                 small circle above note indicating open string or harmonic
!thumb!                cello thumb symbol
!breath!               a breath mark (apostrophe-like) after note
!pppp! !ppp! !pp! !p!  dynamics marks
!mp! !mf! !f! !ff!     more dynamics marks
!fff! !ffff! !sfz!     more dynamics marks
!crescendo(!           start of a < crescendo mark
!<(!                   same as !crescendo(!
!crescendo)!           end of a < crescendo mark, placed after the last note
!<)!                   same as !crescendo)!
!diminuendo(!          start of a > diminuendo mark
!>(!                   same as !diminuendo(!
!diminuendo)!          end of a > diminuendo mark, placed after the last note
!>)!                   same as !diminuendo)!
!segno!                2 ornate s-like symbols separated by a diagonal line
!coda!                 a ring with a cross in it
!D.S.!                 the letters D.S. (=Da Segno)
!D.C.!                 the letters D.C. (=either Da Coda or Da Capo)
!dacoda!               the word "Da" followed by a Coda sign
!dacapo!               the words "Da Capo"
!fine!                 the word "fine"
!shortphrase!          vertical line on the upper part of the staff
!mediumphrase!         same, but extending down to the centre line
!longphrase!           same, but extending 3/4 of the way down
"""


##======================================================================
class Token(object):
    def __init__(self, line, char, text):
        self._line = line
        self._char = char
        self._text = text

    def __repr__(self):
        return "<%s %s>" % (self.__class__.__name__, repr(self._text))

    def __str__(self):
        return self._text

    def to_json(self):
        """Return a JSON-safe dictionary representing this token"""
        return {'text':self._text, 'line':self._line, 'char':self._char, 'type':type(self).__name__}

class Extended(Token):
    """Token with a length (Pitch or Rest)"""
    def __init__(self, time, num, denom, **kwds):
        Token.__init__(self, **kwds)
        self.time_sig = time
        self._length = (num, denom)

    @property
    def length(self):
        """Event length in tics (rational tuple)"""
        n,d = self._length
        return (int(n) if n is not None else 1, int(d) if d is not None else 1)

    @property
    def duration(self):
        """Event duration in tics (float)"""
        return self.length[0] / self.length[1]

    @staticmethod
    def _abc_length(num, denom):
        """Return abc length suffix for a length tuple"""
        lf = Fraction(num, denom)
        if lf.numerator == 1:
            if lf.denominator == 1:
                return ''
            elif lf.denominator == 2:
                return '/'
            else:
                return f'/{lf.denominator}'
        elif lf.denominator == 1:
            return str(lf.numerator)
        else:
            return f'{lf.numerator}/{lf.denominator}'

    def abc_length(self):
        """Return abc length suffix for this event"""
        return self._abc_length(*self.length)

    def stretch(self, frac):
        """Stretch length by Fraction ``frac``"""
        self._length = (Fraction(*self.length) * frac).as_integer_ratio()

    def to_json(self):
        data = super().to_json()
        data['length'] = list(self.length)
        data['duration'] = self.duration
        return data

class Note(Extended):
    def __init__(self, key, time, note, accidental, octave, num, denom, **kwds):
        Extended.__init__(self, time, num, denom, **kwds)
        self.key = key
        self.note = note
        self.accidental = accidental
        self.octave = octave

    @property
    def pitch(self):
        """Chromatic note value taking into account key signature and transpositions.
        """
        return Pitch(self)

    def abc_canonical(self):
        #TODO: fix duration suffix for tuplets
        # - abcm2ps says "Bad length divisor" for e.g. "(3A4/9B4/9c4/9 "
        # - should really map to just "(3ABC"
        return (self.accidental or '') + self.pitch.abc_pitch(accidentals=False) + self.abc_length()

    def stretch(self, frac):
        """Stretch length by Fraction ``frac``"""
        super().stretch(frac)
        self._text = self.abc_canonical()

    def dotify(self, dots, direction):
        """Apply dot(s) to the duration of this note.
        """
        assert direction in ('left', 'right')
        longer = direction == 'left'
        if '<' in dots:
            longer = not longer
        n_dots = len(dots)
        num, den = self.length
        if longer:
            num = num * 2 + 1
            den = den * 2
            self._length = (num, den)
        else:
            den = den * 2
            self._length = (num, den)

    def to_json(self):
        data = super().to_json()
        data['key'] = self.key.to_json()
        data['note'] = self.note
        data['accidental'] = self.accidental
        data['octave'] = self.octave
        data['pitch'] = self.pitch.to_json()
        return data



class Beam(Token):
    pass

class WhitespaceToken(Token):
    pass

class Space(WhitespaceToken):
    pass

class Slur(Token):
    """   ( or )   """
    pass

class Tie(Token):
    """   -   """
    pass

class NewlineToken(WhitespaceToken):
    pass

class Newline(NewlineToken):
    pass

class Continuation(NewlineToken):
    """  \\ at end of line  """
    def __str__(self):
        return self._text + '\n'

class GracenoteBrace(Token):
    """  {  {/  or }  """
    pass

class ChordBracket(Token):
    """  [  or  ]  """
    pass

class ChordSymbol(Token):
    """   "Amaj"   """
    pass

class Annotation(Token):
    """    "<stuff"   """
    pass

class Decoration(Token):
    """  .~HLMOPSTuv  """
    pass

class Tuplet(Token):
    """  (5   """
    def __init__(self, num, **kwds):
        Token.__init__(self, **kwds)
        self.num = num

class BodyField(Token):
    def __str__(self):
        return self._text + '\n'

class InlineField(Token):
    @property
    def content(self):
        return self._text.lstrip('[').rstrip(']')

    @property
    def key(self):
        return self.content.split(':', 1)[0]

    @property
    def value(self):
        return self.content.split(':', 1)[1]

    def to_json(self):
        data = super().to_json()
        data['key'] = self.key
        data['value'] = self.value
        return data


class Rest(Extended):
    def __init__(self, symbol, time, num, denom, **kwds):
        # char==X or Z means length is in measures -- should be handled by parse_abc()
        Extended.__init__(self, time, num, denom, **kwds)
        self.symbol = symbol
        self.length_ = (num, denom)

    def stretch(self, frac):
        """Stretch length by Fraction ``frac``"""
        super().stretch(frac)

    def __str__(self):
        if self.symbol.isupper():
            len_ = self.length
            return self.symbol + Extended._abc_length(int(len_[0] / self.time_sig.bar_length), len_[1])
        else:
            return self.symbol + self.abc_length()

##======================================================================
class InfoContext(object):
    """Keeps track of current information fields
    """
    def __init__(self, fields):
        self._fields = fields

    def __getattr__(self, field):
        return self._fields.get(field, None)

    def copy(self, fields):
        """Return a copy with some fields updated
        """
        f2 = InfoContext(self._fields)
        f2._fields.update(fields)
        return f2

##======================================================================
class Tune(object):
    """Initialize with either an ABC string or a json-parsed dict read from
    the TheSession API.
    """
    def __init__(self, abc=None, json=None):
        if abc is not None:
            self.parse_abc(abc)
        elif json is not None:
            self.parse_json(json)
        else:
            raise TypeError("must provide abc or json")

    @property
    def url(self):
        try:
            return "http://thesession.org/tunes/%d#setting%d" % (self.header['reference number'], self.header['setting'])
        except KeyError:
            return None

    @property
    def notes(self):
        return [t for t in self.tokens if isinstance(t, Note)]

    def parse_abc(self, abc):
        self.abc = abc
        header = []
        tune = []
        in_tune = False
        for line in abc.split('\n'):
            line = re.split(r'([^\\]|^)%', line)[0]
            line = line.strip()
            if line == '':
                continue
            if in_tune:
                tune.append(line)
            else:
                if line[0] in info_keys and line[1] == ':':
                    header.append(line)
                    if line[0] == 'K':
                        in_tune = True
                elif line[:2] == '+:':
                    header[-1] += ' ' + line[2:]

        self.parse_header(header)
        self.parse_tune(tune)

    def parse_json(self, json):
        self.header = {
            "reference number": json['tune'],
            "setting": json['setting'],
            "tune title": json['name'],
            "meter": json['meter'],
            "unit note length": "1/" + json['meter'].split('/')[1],
            "key": json['mode'],
        }
        self.parse_tune(json['abc'].split('\r\n'))

    def parse_header(self, header):
        h = {}
        for line in header:
            key = line[0]
            data = line[2:].strip()
            h[info_keys[key].name] = data
        self.header = h
        self.reference = h['reference number']
        self.title = h['tune title']
        self.key = h.get('key', 'C')

    def parse_tune(self, tune):
        self.tokens = self.tokenize(tune, self.header)

    def tokenize(self, tune, header):
        # get initial key signature from header
        key = Key(self.header['key'])

        # get initial time signature from header
        meter = self.header.get('meter', 'free')
        unit = self.header.get('unit note length', None)
        # determine default unit note length from meter if possible
        if unit is None and meter != 'free':
            if eval(meter) < 0.75:
                unit = "1/16"
            else:
                unit = "1/8"
        tempo = self.header.get('tempo', None)
        time_sig = TimeSignature(meter, unit, tempo)
        setattr(self, 'time_sig', time_sig)


        tokens = []
        for i,line in enumerate(tune):
            #print(line)
            line = line.rstrip()

            if len(line) > 2 and line[1] == ':' and (line[0] == '+' or line[0] in tune_body_fields):
                tokens.append(BodyField(line=i, char=0, text=line))
                continue

            pending_dots = None
            j = 0
            while j < len(line):
                part = line[j:]

                # Field
                if part[0] == '[' and len(part) > 3 and part[2] == ':':
                    fields = ''.join(inline_fields.keys())
                    m = re.match(r'\[[%s]:([^\]]+)\]' % fields, part)
                    if m is not None:
                        if m.group()[1] == 'K':
                            key = Key(m.group()[3:-1])

                        tokens.append(InlineField(line=i, char=j, text=m.group()))
                        j += m.end()
                        continue

                # Space
                m = re.match(r'(\s+)', part)
                if m is not None:
                    tokens.append(Space(line=i, char=j, text=m.group()))
                    j += m.end()
                    continue

                # Note
                # Examples:  c  E'  _F2  ^^G,/4  =a,',3/2
                m = re.match(r"(?P<acc>\^|\^\^|=|_|__)?(?P<note>[a-gA-G])(?P<oct>[,']*)(?P<num>\d+)?(?P<slash>/+)?(?P<den>\d+)?", part)
                if m is not None:
                    g = m.groupdict()
                    octave = int(g['note'].islower())
                    if g['oct'] is not None:
                        octave -= g['oct'].count(",")
                        octave += g['oct'].count("'")

                    num = g.get('num', 1)
                    if g['den'] is not None:
                        denom = g['den']
                    elif g['slash'] is not None:
                        denom = 2 * g['slash'].count('/')
                    else:
                        denom = 1

                    tokens.append(Note(key=key, time=time_sig, note=g['note'], accidental=g['acc'],
                        octave=octave, num=num, denom=denom, line=i, char=j, text=m.group()))

                    if pending_dots is not None:
                        tokens[-1].dotify(pending_dots, 'right')
                        pending_dots = None

                    j += m.end()
                    continue

                # Beam  |   :|   |:   ||   and Chord  [ABC]
                m = re.match(r'([\[\]\|\:]+)([0-9\-,])?', part)
                if m is not None:
                    if m.group() in '[]':
                        tokens.append(ChordBracket(line=i, char=j, text=m.group()))
                    else:
                        tokens.append(Beam(line=i, char=j, text=m.group()))
                    j += m.end()
                    continue

                # Broken rhythm
                if len(tokens) > 0 and isinstance(tokens[-1], (Note, Rest)):
                    m = re.match('<+|>+', part)
                    if m is not None:
                        tokens[-1].dotify(part, 'left')
                        pending_dots = part
                        j += m.end()
                        continue

                # Rest
                m = re.match(r'([XZxz])(\d+)?(/(\d+)?)?', part)
                if m is not None:
                    g = m.groups()
                    num = int(g[1]) if g[1] is not None else 1
                    if g[0].isupper():
                        # char==X or Z means length is in measures
                        num *= time_sig.bar_length
                    tokens.append(Rest(g[0], time=time_sig, num=num, denom=g[3], line=i, char=j, text=m.group()))

                    if pending_dots is not None:
                        tokens[-1].dotify(pending_dots, 'right')
                        pending_dots = None

                    j += m.end()
                    continue

                # Tuplets  (must parse before slur)
                m = re.match(r'\(([2-9])', part)
                if m is not None:
                    tokens.append(Tuplet(num=m.groups()[0], line=i, char=j, text=m.group()))
                    j += m.end()
                    continue

                # Slur
                if part[0] in '()':
                    tokens.append(Slur(line=i, char=j, text=part[0]))
                    j += 1
                    continue

                # Tie
                if part[0] == '-':
                    tokens.append(Tie(line=i, char=j, text=part[0]))
                    j += 1
                    continue

                # Embelishments
                m = re.match(r'(\{\\?)|\}', part)
                if m is not None:
                    tokens.append(GracenoteBrace(line=i, char=j, text=m.group()))
                    j += m.end()
                    continue

                # Decorations (single character)
                if part[0] in '.~HLMOPSTuv':
                    tokens.append(Decoration(line=i, char=j, text=part[0]))
                    j += 1
                    continue

                # Decorations (!symbol!)
                m = re.match(r'\!([^\! ]+)\!', part)
                if m is not None:
                    tokens.append(Decoration(line=i, char=j, text=m.group()))
                    j += m.end()
                    continue

                # Continuation
                #if j == len(line) - 1 and j == '\\':
                if re.match(r'\s*\\\s*$', part):
                    tokens.append(Continuation(line=i, char=j, text='\\'))
                    j += 1
                    continue

                # Annotation
                m = re.match(r'"[\^\_\<\>\@][^"]+"', part)
                if m is not None:
                    tokens.append(Annotation(line=i, char=j, text=m.group()))
                    j += m.end()
                    continue

                # Chord symbol
                m = re.match(r'"[\w#/]+"', part)
                if m is not None:
                    tokens.append(ChordSymbol(line=i, char=j, text=m.group()))
                    j += m.end()
                    continue

                raise Exception("Unable to parse: %s\n%s" % (part, self.url))

            if not isinstance(tokens[-1], Continuation):
                tokens.append(Newline(line=i, char=j, text='\n'))

        ##-- normalize tuplet durations
        tuplet_offsets = [
            i for i in range(len(tokens))
            if isinstance(tokens[i], Tuplet)
        ]
        for i in tuplet_offsets:
            tuplet_num = int(tokens[i].num)
            tuplet_count = 0
            for j in range(i + 1, len(tokens)):
                if isinstance(tokens[j], Extended):
                    tuplet_count += 1
                    note_length = list(tokens[j].length)
                    ##-- TODO: wonky for 5-tuplets etc.?
                    note_length[0] *= round(0.25 / self.time_sig.unit_length) # *= 2
                    note_length[1] *= tuplet_num
                    tokens[j]._length = tuple(note_length)
                if tuplet_count >= tuplet_num:
                    break

        return tokens

    def pitchogram(tune):
        hist = {}
        for note in tune.notes:
            v = note.pitch.abs_value
            hist[v] = hist.get(v, 0) + note.duration
        return hist

    def body(self):
        """'canonical' abc body; from self.tokens"""
        return ''.join(map(str, self.tokens))

    def head(self, include_reference=True):
        """abc header, from self.abc"""
        return '\n'.join(filter(
            lambda l: include_reference or (not l.startswith('X')),
            [
                line for line in self.abc.split('\n')
                if re.match(r'^[A-Z]\s*:', line)
            ]))

    def as_phrase(self):
        return Phrase(self)

    def __str__(self):
        return f'{self.head()}\n{self.body()}'.strip() + '\n'

    def __repr__(self):
        return f'<{type(self).__name__} #{self.reference}: {repr(self.title)}>'

    def imply_parts(self):
        """
        Adds [P:] labels to self.tokens after || bars, only if no labels are present.
        Invalidates unsevered phrases and self.abc.
        """
        part_bars = []
        has_part_bars = any(str(tok).endswith('||') for tok in self.tokens)
        def is_part_bar(tok):
            if not isinstance(tok, Beam):
                return False
            elif has_part_bars:
                return str(tok).endswith('||')
            else:
                return re.search(r'[:|][\]\|]$', str(tok))
        for j in range(len(self.tokens)):
            tok = self.tokens[j]
            if isinstance(tok, InlineField) and tok.key == 'P':
                return self
            elif is_part_bar(tok):
                part_bars.append(j)
        if not part_bars:
            return self

        # add part labels after every part bar
        part_bars = list(zip(
            [-1, *part_bars],
            [chr(ord('A') + i) for i in range(len(part_bars) + 1)]
        ))
        for offset, label in reversed(part_bars):
            for x in (
                Space(-1, -1, f' '), # othewise parse errors ':|['
                InlineField(-1, -1, f'[P:{label}]'),
                Space(-1, -1, f' '), # othewise parse errors ':|['
            ):
                self.tokens.insert(offset + 1, x)
        return self

    def stretch(self, num=2, denom=1):
        """
        Scale all note lengths by ``num/denom``.
        Also tweaks the time signature.
        """
        # stretch tokens
        frac = Fraction(num, denom)
        for tok in self.tokens:
            if isinstance(tok, Extended):
                tok.stretch(frac)

        # stretch tune time signature
        seen = {id(self.time_sig)}
        self.time_sig.stretch_meter(frac)
        for tok in self.tokens:
            if isinstance(tok, Extended) and id(tok.time_sig) not in seen:
                tok.time_sig.stretch_meter(frac)

        # tweak abc header
        lines = []
        changed = False
        for line in self.abc.split('\n'):
            if re.match('^[ML]:', line):
                if not changed:
                    lines.append(str(self.time_sig))
                    changed = True
            else:
                lines.append(line)
        self.abc = '\n'.join(lines)

        return self



##======================================================================
class Phrase:
    def __init__(self, tune, start=0, end=None, label=None):
        self.tune = tune
        self.label = label if label is not None else tune.title
        self.start = start
        self.end = end if end is not None else len(tune.tokens) if tune is not None else None

    def sever(self, trim=False):
        """
        Sever from original tune (copies).
        If ``trim`` is true, restricts tune tokens to the selected range.
        """
        self.tune = copy.deepcopy(self.tune)
        if trim:
            self.tune.tokens = self.tune.tokens[self.start:self.end]
            self.start = 0
            self.end = len(self.tune.tokens)
        return self

    def as_tune(self):
        """Promote to tune"""
        tune = copy.deepcopy(self.tune)
        tune.tokens = self.tune.tokens[self.start:self.end]
        tune.title += f' ({self.label})'
        tune.abc = self.abc
        return tune

    @property
    def tokens(self):
        if self.tune is None:
            return []
        return self.tune.tokens[self.start:self.end]

    @property
    def abc(self):
        tokens = self.tokens
        for i in range(len(self)):
            if not str(tokens[i]).isspace():
                break
        return ''.join(map(str, tokens[i:]))

    def __len__(self):
        return self.end - self.start

    def __bool__(self):
        return len(self) != 0

    def __repr__(self):
        return f'{type(self).__name__}(X{self.tune.reference}[{self.start}:{self.end}], {repr(self.label)})'

    def head(self, *args, **kwargs):
        return '\n'.join([
            f'X:{self.xid}',
            f'T:{self.label}',
            str(self.tune.time_sig),
            f'K:{self.tune.key}',
        ]).strip()

    def body(self):
        return self.abc.strip()

    def __str__(self):
        return f'{self.head()}\n{self.body()}' + '\n'

    @property
    def duration(self):
        """Duration in beat units"""
        return sum([
            x.duration for x in self.tokens
            if hasattr(x, 'duration')
        ])

    _id2x = {None: 0}
    @property
    def xid(self):
        try:
            return self._id2x[id(self)]
        except KeyError:
            pass
        self._id2x[id(self)] = len(self._id2x)
        return self._id2x[id(self)]


    def slice(self, start=0, end=None, label=None):
        """start, end are relative to self"""
        end = end if end is not None else len(self)
        label = f'{self.label}[{start}:{end}]' if label is None else label
        return type(self)(self.tune, self.start+start, self.start+end, label)

    def by_part(self, label=''):
        """return a list of sub-phrases by part: (label, subphrase)"""
        parts = []
        tokens = self.tokens
        i = 0
        for j in range(i, len(tokens)):
            tok = tokens[j]
            if isinstance(tok, InlineField) and str(tok).startswith('[P:'):
                iend = preceding_bar(tokens, j)
                if iend > i:
                    parts.append((label, self.slice(i, iend + 1)))
                    i = iend + 1
                label = re.match(r'\[P:(.*)\]', str(tok)).group(1).strip()
        # final
        if i < len(tokens):
            parts.append((label, self.slice(i)))
        return parts

    def bars(self):
        """
        yield a stream of subphrases separted by bars.
        Bar tokens are included with preceding content.
        No length checking.
        """
        tokens = self.tokens
        start = 0
        bar = 0
        while start < len(tokens):
            end = following_bar(tokens, start) + 1
            yield self.slice(start, end, label=f'{self.label}:b{bar}')
            start = end
            bar += 1

    # TODO: better partial bar handling
    # - return partial bars as their own objects (give then fractional offset keys)?
    def by_bar(self):
        """
        return a dictionary from bar-numbers to sub-phrases
        - first full bar has bar-number 1
        - partial intro bar(s) (if present) have bar-number 0
        #- non-initial partial bars are attached to preceding bar
        #- bar numbers are only incremented on full bars (according to tune timesig)
        """
        tokens = self.tokens
        bars_raw = list(self.bars())
        bar_length = self.tune.time_sig.bar_length
        bar_number = 0
        bars = {}
        for bar in bars_raw:
            if bar_number == 0 and bar.duration < bar_length:
                # intro bar(s)
                bar.label = re.sub(r':b\d+$', f':b{bar_number}', bar.label)
                try:
                    bars[0].end = bar.end
                except KeyError:
                    bars[0] = bar
            #elif bar.duration < bar_length:
            #    # partial bar: append to left neighbor
            #    bars[bar_number - 1].end = bar.end
            else:
                # complete bar
                bar_number = max(1, bar_number)
                bars[bar_number] = bar
                bar.label = re.sub(r':b\d+$', f':b{bar_number}', bar.label)
                bar_number += 1
        return bars

    def by_tic(self, start=1):
        """
        Yield a stream of (tic, token) pairs.
        Resets tic on bar boundaries.
        """
        tic = start
        in_chord = 0
        chord_tics = []
        for tok in self.tokens:
            yield (tic, tok)
            if isinstance(tok, Beam):
                tic = start
            elif isinstance(tok, ChordBracket):
                if str(tok) == '[':
                    in_chord = True
                    chord_tics.clear()
                else:
                    in_chord = False
                    tic += max(chord_tics)
            elif isinstance(tok, Extended):
                if in_chord:
                    chord_tics.append(tok.duration)
                else:
                    tic += tok.duration

    def prune(self, want=None, default=False):
        """
        Return a new trim-severed Phrase containing only those abc tokens
        for which ``want.get(typename, default)`` returns true.
        """
        if want is None:
            want = {
                'Note': True,
                'Rest': True,
                'Beam': True,
                'ChordSymbol': True,
                'InlineField': False,
                'BodyField': False,
                'Space': True,
                'Newline': True,
                'Continuation': True,
            }
        pruned = self.sever(trim=True)
        pruned.label += ':prune'
        tokens = []
        last_was_space = False
        for tok in pruned.tune.tokens:
            if isinstance(tok, Space) and last_was_space:
                # skip repeated spaces
                continue
            elif want.get(type(tok).__name__, default):
                tokens.append(tok)
            elif isinstance(tok, NewlineToken) and want.get('Space', default) and not last_was_space:
                # bash newlines & continuations to spaces
                tokens.append(Space(-1, -1, ' '))
            last_was_space = isinstance(tok, WhitespaceToken)
        pruned.tune.tokens = tokens
        return pruned

    def melody(self):
        """Generate a stream of melodic tokens (notes and rests)"""
        return filter(lambda tok: isinstance(tok, Extended), self.tokens)



##======================================================================
class Grid:
    def __init__(self, tune_or_phrase):
        if isinstance(tune_or_phrase, Tune):
            self.tune = tune_or_phrase
            self.phrase = Phrase(self.tune)
        else:
            self.tune = tune_or_phrase.tune
            self.phrase = tune_or_phrase
        self.grid = {} # grid[bar_number][tic_number] = list(tokens)
        self.bootstrap()

    @property
    def tokens(self):
        return self.phrase.tokens

    @property
    def abc(self):
        return self.phrase.abc

    def __repr__(self):
        return f'{type(self).__name__}(' + repr(self.phrase) + ')'

    def bootstrap(self):
        self.grid.clear()
        def qtic(tic):
            return int(round(tic, 5))
        for bar_num, bar in self.phrase.by_bar().items():
            bar_grid = self.grid.setdefault(bar_num, {})
            for tic_onset, token in bar.by_tic():
                for tic in range(qtic(tic_onset), qtic(tic_onset + max(1, getattr(token, 'duration', 1)))):
                    bar_grid.setdefault(tic, []).append(token)
        return self

##======================================================================
## moo: utils

def preceding_bar(tokens, offset):
    """Return index of last Beam at-or-before offset (inclusive), or -1"""
    for j in range(offset, -1, -1):
        if isinstance(tokens[j], Beam):
            return j
    return -1

def following_bar(tokens, offset):
    """Return index of first Beam at-or-after offset, or len(tokens)"""
    for j in range(offset, len(tokens)):
        if isinstance(tokens[j], Beam):
            return j
    return len(tokens)


##======================================================================
def get_thesession_tunes():
    import os, json
    if not os.path.isfile("tunes.json"):
        import sys, urllib
        url = 'https://raw.githubusercontent.com/adactio/TheSession-data/master/json/tunes.json'
        print("Downloading tunes database from %s..." % url)
        try:
            urllib.urlretrieve(url, 'tunes.json')
        except AttributeError:
            import urllib.request
            urllib.request.urlretrieve(url, 'tunes.json')
    return json.loads(open('tunes.json', 'rb').read().decode('utf8'))


##======================================================================
if __name__ == '__main__':
    ts_tunes = get_thesession_tunes()
    for i,t in enumerate(ts_tunes):
        print("----- %d: %s -----" % (i, t['name']))
        tune = Tune(json=t)

    print("Header: %s" % tune.header)


    def show(tune):
        import pyqtgraph as pg
        plt = pg.plot()
        plt.addLine(y=0)
        plt.addLine(y=12)
        plt.addLine(x=0)

        ticks = []
        for i in (0, 1):
            for pitch in "CDEFGAB":

                ticks.append((i*12 + pitch_values[pitch], pitch))

        plt.getAxis('left').setTicks([ticks])

        tvals = []
        yvals = []

        t = 0
        for token in tune.tokens:
            if isinstance(token, Beam):
                plt.addLine(x=t)
            elif isinstance(token, Note):
                tvals.append(t)
                yvals.append(token.pitch.abs_value)
                t += token.duration
        plt.plot(tvals, yvals, pen=None, symbol='o')


        hist = tune.pitchogram()
        k = sorted(hist.keys())
        v = [hist[x] for x in k]
        plt = pg.plot()
        bar = pg.BarGraphItem(x=k, height=v, width=1)
        plt.addItem(bar)

        plt.getAxis('bottom').setTicks([ticks])
