import fontforge
import sys

origfont = fontforge.open(sys.argv[1])
overwrite = fontforge.open(sys.argv[2])

overwrite.selection.all()
origfont.selection.none()
for s in overwrite.selection:
    origfont.selection.select(('more',), s)

overwrite.copy()
origfont.paste()

origfont.generate(sys.argv[3])
