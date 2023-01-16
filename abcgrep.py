#!/usr/bin/python3

import click
import copy
import pyabc
import re

from pyabc import Tune, Phrase, Grid

##======================================================================
## utils

##--------------------------------------------------------------
def load_tunes(filename_or_stream):
    if isinstance(filename_or_stream, str):
        stream = open(filename_or_stream, 'r', encoding='utf-8')
    else:
        stream = filename_or_stream
    lines = []
    tunes = []

    line_started = 0
    lineno = 0
    def add_tune(line_started):
        try:
            if lines and not all(map(lambda l: l.startswith('%') or l.isspace(), lines)):
                #tunes.append(''.join(lines))
                tunes.append(pyabc.Tune(''.join(lines)))
        except Exception as err:
            click.echo(f'Error parsing tune at lines {line_started}-{lineno}: {str(err)}', err=True)
            raise err
        lines.clear()

    for line in stream:
        lineno += 1
        is_blank = re.match(r'^\s*$', line)
        if re.match(r'^[xX]:', line) or is_blank:
            add_tune(line_started)
            line_started = lineno
        if lines or not is_blank:
            lines.append(line)
    add_tune(line_started)
    return tunes

##--------------------------------------------------------------
def parse_ranges(ranges_str, expand=False):
    """
    Parse a range-list string into a list of objects (ints or tuples).

    Input should be a space- or comma-separated list of items of the following form:
    - VAL - single item, mapped to int
    - MIN:MAX - maps to tuple (MIN,MAX+1)   : inclusive

    If expand is true, output will contain only int values.
    """
    if not ranges_str:
        return ()
    ranges = []
    for val in re.split(r'[\s\,]+', ranges_str):
        if ':' in val:
            vmin, vmax = val.split(':', 1)
            ranges.append((int(vmin), int(vmax)+1))
        else:
            ranges.append(int(val))
    return list(expand_ranges(ranges)) if expand else ranges

##--------------------------------------------------------------
def expand_ranges(ranges):
    """Generator to expand a range-list to all integer items"""
    for val in ranges:
        if isinstance(val, tuple):
            yield from range(val[0], val[1])
        else:
            yield val

##======================================================================
## group: main
click_settings = dict(help_option_names=["-h", "--help"])
@click.group(context_settings=click_settings)
def cli():
    """Filter abc notation files"""
    pass

##======================================================================
## command: tunes
@cli.command()
@click.option('-e', '-x', '--id', 'ids', type=str, default=None,
              help='selection list (space- or comma-separated list of ranges): reference (X:)')
@click.option('-r', '--reference', type=str, help='selection regex: reference (X:)')
@click.option('-a', '--abc', type=str, help='selection regex: abc content')
@click.option('-t', '--title', type=str, help='selection regex: title')
@click.option('-k', '--key', type=str, help='selection regex: key')
@click.argument('abcfile', type=click.File('r', encoding='utf8'), default='-')
def tunes(abcfile, ids, reference, abc, title, key):
    """
    Select tunes from ABCFILE based on ID and/or property regexes.
    All conditions must be met.
    """
    tunes = load_tunes(abcfile)
    selected = list(select_tunes(tunes,
                                 ids=parse_ranges(ids),
                                 reference=reference,
                                 abc=abc,
                                 title=title,
                                 key=key))
    print('\n'.join(map(lambda tune: tune.abc, selected)))

##--------------------------------------------------------------
def select_tunes(tunes, ids=(), **kwargs):
    """
    generator: filters tunes based on regex conditions in **kwargs.
    Known kwargs fields:
    - ids (abc "X": iterable)
    - reference (abc "X")
    - title
    - key
    - abc
    - header (JSON)
    - tokens (reprs)
    """
    ids = set(map(str, expand_ranges(ids))) if ids else None
    conditions = {key: val for key, val in kwargs.items() if val is not None}
    if not ids and not conditions:
        yield from tunes
        return

    def check_condition(tune, c):
        return re.search(conditions[c], getattr(tune, c))

    for tune in tunes:
        if all([
                (not ids or str(tune.reference) in ids),
                *map(lambda c: check_condition(tune, c), conditions)
        ]):
            yield tune

##======================================================================
## command: phrases
@cli.command()
@click.option('-e', '-x', '--id', 'ids', type=str, default='',
              help='tune selection list (ranges): reference (X:)')
@click.option('-p', '--part', type=str, help='selection regex: part (P:)')
@click.option('-b', '--bar', type=str,
              help='selection ranges: bar number (ranges)')
@click.argument('abcfile', type=click.File('r', encoding='utf8'), default='-')
def phrases(abcfile, ids, part, bar):
    """
    Select phrases from ABCFILE based on location.
    """
    tunes = load_tunes(abcfile)
    tunes = list(select_tunes(tunes, ids=parse_ranges(ids)))
    phrases = select_phrases(
        tunes,
        part=part,
        bar=parse_ranges(bar),
    )
    for phrase in phrases:
        print(phrase)

##--------------------------------------------------------------
def select_phrases(tunes, part=None, bar=None, **kwargs):
    """
    select phrases from multiple tunes.
    returns a list of Phrase objects by (in order of decomposition):
    - part (regex)
    - bar (offsets or (min,max) ranges)
    - beat (offsets or (min,max) ranges)
    """

    ##-- phrases: by tune
    phrases = [
        Phrase(tune.imply_parts(), label=f'#{tune.reference}').sever()
        for tune in tunes
    ]

    ##-- phrases: by part
    if part is not None:
        buf = []
        for p in phrases:
            buf.extend(part_phrases(p, part_re=part, default=''))
        phrases = buf

    ##-- phrases by bar (ranges)
    if bar:
        buf = []
        for p in phrases:
            buf.extend(part_bars(p, bar_ranges=bar))
        phrases = buf

    ##-- phrases by beat (rangges)
    #TODO

    return phrases

##--------------------------------------------------------------
def part_phrases(parent_phrase, part_re, default=''):
    """
    yields a stream of Phrase objects whose part label matches part_re
    """
    for label, phrase in parent_phrase.by_part(default):
        if re.search(part_re, label):
            phrase.label += f':{label}'
            yield phrase

##--------------------------------------------------------------
def part_bars(parent_phrase, bar_ranges):
    """
    yields a stream of Phrase objects from parent_phrase for each range of measures in bar_ranges.
    """
    by_bar = parent_phrase.by_bar()
    for rng in bar_ranges:
        if isinstance(rng, int) and rng in by_bar:
            yield by_bar[rng]
        elif isinstance(rng, tuple) and all(b in by_bar for b in range(*rng)):
            bars = copy.copy(by_bar[rng[0]])
            bars.end = by_bar[rng[1] - 1].end
            bars.label = re.sub(r':b\d+$', rf'#b{rng[0]}:{rng[1]}', bars.label)
            yield bars

##======================================================================
## command: jsonl
@cli.command()
@click.option('-e', '-x', '--id', 'ids', type=str, default='',
              help='tune selection list (ranges): reference (X:)')
@click.option('-o', '--output', type=click.File('w', encoding='utf8'), default='-',
              help='output file')
@click.argument('abcfile', type=click.File('r', encoding='utf8'), default='-')
def jsonl(abcfile, output, ids):
    """
    Print verbose tune abc tokens as JSONL (1 token per line).
    """
    tunes = load_tunes(abcfile)
    tunes = list(select_tunes(tunes, ids=parse_ranges(ids)))
    import json
    for tune in tunes:
        tune.imply_parts()
        tune_info = {'reference': tune.reference, 'title': tune.title, 'header': tune.header, 'key': tune.key}
        for ctx, token in verbose_tokens(tune.tokens, {'key':tune.key, 'bar':1}):
            row = token.to_json()
            row['tune'] = tune_info
            row['context'] = ctx
            json.dump(row, output, separators=(',', ':'))
            output.write('\n')
        output.write('\n')

##--------------------------------------------------------------
def verbose_tokens(tokens, defaults=None):
    """
    yield a stream of ``meta, token`` pairs. -- DROP?
    metadata keys:
    - part: NAME (str)
    - bar: NUM (int)
    - key: KEY (str)
    - line: LINE (int)
    - chord: CHORD (str)
    #TODO: beat (tic)
    #  - or break down into float "{beat}.{btic}" or pair [beat,btic]
    """
    if defaults is None:
        attrs = {}
    else:
        attrs = copy.copy(defaults)
    attrs.setdefault('part', None)
    attrs.setdefault('bar', 0)
    attrs.setdefault('line', 0)
    attrs.setdefault('key', None)
    attrs.setdefault('chord', None)

    for tok in tokens:
        if isinstance(tok, (pyabc.InlineField, pyabc.BodyField)) and str(tok).startswith('[P:'):
            ##-- part label
            attrs['part'] = re.match(r'\[P:(.*)\]', str(tok)).group(1).strip()
        elif isinstance(tok, pyabc.BodyField) and str(tok).startswith('K:'):
            ##-- key change
            attrs['key'] = str(tok)[3:].strip()
        elif isinstance(tok, pyabc.Beam):
            ##-- measure bar
            attrs['bar'] += 1
        elif isinstance(tok, pyabc.Newline):
            ##-- newline
            attrs['line'] += 1
        elif isinstance(tok, pyabc.ChordSymbol):
            attrs['chord'] = str(tok).strip('"')
        yield copy.copy(attrs), tok


##======================================================================
## command: prune
@cli.command()
@click.option('-e', '-x', '--id', 'ids', type=str, default='',
              help='tune selection list (ranges): reference (X:)')
@click.option('-n/-N', '--notes/--no-notes', type=bool, default=True,
              help="Do/don't include notes & rests (default=do)")
@click.option('-b/-B', '--bars/--no-bars', type=bool, default=True,
              help="Do/don't include bars/beams (default=don't)")
@click.option('-c/-C', '--chords/--no-chords', type=bool, default=True,
              help="Do/don't nclude chord symbols (default=do)")
@click.option('-s/-S', '--spaces/--no-spaces', type=bool, default=True,
              help="Do/don't include spaces (default=do)")
@click.option('-l/-L', '--lines/--no-lines', type=bool, default=True,
              help="Do/don't include newlines & continuations (default=do)")
@click.option('-f/-F', '--fields/--no-fields', type=bool, default=False,
              help="Do/don't include inline & body fields (default=don't)")
@click.option('-d/-D', '--decorations/--no-decorations', type=bool, default=False,
              help="Do/don't include decorations, slurs, ties, gracenotes etc. (default=don't)")
#TODO: Slur, Tie, Annotation, Tuplet
@click.argument('abcfile', type=click.File('r', encoding='utf8'), default='-')
def prune(abcfile, ids, notes, bars, chords, spaces, lines, fields, decorations):
    """
    Prune abc content tokens.
    """
    tunes = load_tunes(abcfile)
    tunes = list(select_tunes(tunes, ids=parse_ranges(ids)))

    for tune in tunes:
        phrase = Phrase(tune).prune({
            'Note': notes,
            'Beam': bars,
            'ChordSymbol': chords,
            'ChordBracket': chords,
            'Space': spaces,
            'Newline': lines,
            'Continuation': lines,
            'InlineField': fields,
            'BodyField': fields,
        }, default=decorations)
        print(str(phrase))


##======================================================================
## command: tokens
@cli.command()
@click.option('-e', '-x', '--id', 'ids', type=str, default='',
              help='tune selection list (ranges): reference (X:)')
@click.argument('abcfile', type=click.File('r', encoding='utf8'), default='-')
def tokens(abcfile, ids):
    """
    Print abc content tokens (see also 'prune')
    - 1 token per line
    - tunes separated by a blank line
    """
    tunes = load_tunes(abcfile)
    tunes = list(select_tunes(tunes, ids=parse_ranges(ids)))
    for tune in tunes:
        for tok in tune.tokens:
            if isinstance(tok, (pyabc.Space, pyabc.Newline, pyabc.Continuation)):
                continue
            print(str(tok))
        print('')


##======================================================================
## command: cat
@cli.command()
@click.option('-e', '-x', '--id', 'ids', type=str, default='',
              help='tune selection list (ranges): reference (X:)')
@click.option('-m/-M', '--meta/--no-meta', type=bool, default=True,
              help="do/don't include metadata comments (default=do)")
@click.argument('abcfile', type=click.File('r', encoding='utf8'), default='-')
def cat(abcfile, ids, meta):
    """
    Concatenate multiple tunes to a single tune.  Only first header is retained.
    """
    tunes = load_tunes(abcfile)
    tunes = list(select_tunes(tunes, ids=parse_ranges(ids)))
    last_was_newline = True

    for i, tune in enumerate(tunes):
        if i == 0:
            if not meta:
                print(Phrase(tune).head())
            else:
                print(tune.head())
        if i != 0:
            if not last_was_newline:
                print('\n')
            if meta:
                print('\n'.join(map(lambda l: '%' + l, tune.head().split('\n'))))
            print(pyabc.BodyField(-1, -1, f'K:{tune.key}'), end='')
        print(tune.body(), end='')
        last_was_newline = isinstance(tune.tokens[-1], (pyabc.NewlineToken, pyabc))


##======================================================================
## command: grid
@cli.command()
@click.option('-e', '-x', '--id', 'ids', type=str, default='',
              help='tune selection list (ranges): reference (X:)')
@click.argument('abcfile', type=click.File('r', encoding='utf8'), default='-')
def grid(abcfile, ids):
    """
    (WIP) get a rhythmic grid for tune(s).
    """
    tunes = load_tunes(abcfile)
    tunes = list(select_tunes(tunes, ids=parse_ranges(ids)))

    from pprint import pp
    for tune in tunes:
        ##-- DEBUG
        for tic, token in Phrase(tune).by_tic():
            print(f'{tic} {str(token)}')
        ##--/DEBUG
        grid = Grid(tune)
        print(tune.head())
        pp(grid.grid)

##======================================================================
## TODO
## - pyabc alternatives
##   - checkout abcdb parser -> buried pretty deep
## - pyabc tweaks
##   + [X] fork on github
##   + [~] "canonical" output (not just input abc) from tune.tokens --> __str__ method on tokens
## - [ ] select phrases
##   + [X] by part
##     - [X] implicitly add part-labels (if none ever specified)
##       + idea: add a part label at every "||" (and maybe ":||"?)
##   + [X] by (part &) measure range(s)?
##   + [ ] by (part & measure &) beat-ranges?
##   + [ ] by (part & measure & beat &) n-grams?
## - transform
##   + [ ] "grid" / "typewriter" / "quantize": all notes/rests to unit length
##     - triplets get bogus length=[1,1], duration=1.0
##     - maybe first parse to rhythm-tree?
##   + [ ] re-sample (transform rhythms, e.g. Ryan's-style 16ths to 8ths under L:1/8)
##     - set target note-length?
##     - DO THIS: or just bash-down by integer factor (e.g. 2x: 16ths->8ths, 8ths->quarters, etc.)
##   + [ ] notes-only
##      - [ ] remove gracenotes, bars, inline fields, chords, ...
##   + [ ] transpose (give target key?)
##   + [ ] set chords (or copy progression from other tune):
##     - get-chords | set-chords ?
## - filter
##   + [~] remove metatadata (-> notes only) ~ "cat"
## - counts
##   + [ ] ngrams (phrases?) --> only starting on full beat?  on beat 1 or 3?
##     - raw ngrams can be e.g.: python ./abcgrep.py tokens -n -C - | tt-ngrams.perl -n 2
##   + [ ] measures (phrases)
## - grouping
##   + [ ] by "melodic signature":
##     - (chord_symbol?, start_note, end_note)
##     - (chord_symbol(s)?, start_note, next_note_after_end)
## - [ ] cluster ... on projected content
##   + minhash?  pairwise jaccard?

##======================================================================
## click cruft
if __name__ == "__main__":
    cli()
