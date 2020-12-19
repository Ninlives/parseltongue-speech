import fontforge
import sys

points = list(sys.stdin.read())
origfont = fontforge.open(sys.argv[1])
glyphnames = set()

origfont.selection.none()
for point in points:
    origfont.selection.select(('more',), ord(point))

for glyph in origfont.selection.byGlyphs:
    glyphnames.add(glyph.glyphname)

origfont.selection.all()
for glyph in origfont.selection.byGlyphs:
    if glyph.glyphname not in glyphnames:
        glyph.clear()

origfont.generate(sys.argv[2])
