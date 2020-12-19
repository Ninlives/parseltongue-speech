import fontforge
import sys

points = list(sys.stdin.read())
origfont = fontforge.open(sys.argv[1])
glyphnames = set()

for point in points:
    for glyph in origfont.selection.byGlyphs.select(ord(point)):
        glyphnames.add(glyph.glyphname)

for glyph in origfont.selection.byGlyphs.all():
    if glyph.glyphname not in glyphnames:
        glyph.clear()

origfont.generate(sys.argv[2])
