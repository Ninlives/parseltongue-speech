import fontforge
import sys

points = list(sys.stdin.read())
origfont = fontforge.open(sys.argv[1])

for point in points:
    origfont.selection.select(('more',), ord(point))

origfont.selection.invert()
origfont.clear()
origfont.generate(sys.argv[2])
