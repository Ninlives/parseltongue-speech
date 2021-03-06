from pandocfilters import toJSONFilter, Str
import re

CJK = (r'\u2e80-\u2eff\u2f00-\u2fdf\u3040-\u309f\u30a0-\u30fa\u30fc-'
       r'\u30ff\u3100-\u312f\u3200-\u32ff\u3400-\u4dbf\u4e00'
       r'-\u9fff\uf900-\ufaff')

ANY_CJK = re.compile(r'[{CJK}]'.format(CJK=CJK))

DOTS_CJK = re.compile(
    '([\\.]{{2,}}|\u2026)([{CJK}])'.format(CJK=CJK))  # need to escape { }
FIX_CJK_COLON_ANS = re.compile('([{CJK}])\\:([A-Z0-9\\(\\)])'.format(CJK=CJK))

CJK_QUOTE = re.compile(
    '([{CJK}])([`"\u05f4])'.format(CJK=CJK))  # no need to escape `
QUOTE_CJK = re.compile(
    '([`"\u05f4])([{CJK}])'.format(CJK=CJK))  # no need to escape `
FIX_QUOTE_ANY_QUOTE = re.compile(r'([`"\u05f4]+)(\s*)(.+?)(\s*)([`"\u05f4]+)')

CJK_SINGLE_QUOTE_BUT_POSSESSIVE = re.compile(
    "([{CJK}])('[^s])".format(CJK=CJK))
SINGLE_QUOTE_CJK = re.compile("(')([{CJK}])".format(CJK=CJK))
FIX_POSSESSIVE_SINGLE_QUOTE = re.compile(
    "([{CJK}A-Za-z0-9])( )('s)".format(CJK=CJK))

HASH_ANS_CJK_HASH = re.compile(
    '([{CJK}])(#)([{CJK}]+)(#)([{CJK}])'.format(CJK=CJK))
CJK_HASH = re.compile('([{CJK}])(#([^ ]))'.format(CJK=CJK))
HASH_CJK = re.compile('(([^ ])#)([{CJK}])'.format(CJK=CJK))

CJK_OPERATOR_ANS = re.compile(
    '([{CJK}])([\\+\\-\\*\\/=&\\|<>])([A-Za-z0-9])'.format(CJK=CJK))
ANS_OPERATOR_CJK = re.compile(
    '([A-Za-z0-9])([\\+\\-\\*\\/=&\\|<>])([{CJK}])'.format(CJK=CJK))

FIX_SLASH_AS = re.compile(r'([/]) ([a-z\-_\./]+)')
FIX_SLASH_AS_SLASH = re.compile(r'([/\.])([A-Za-z\-_\./]+) ([/])')

CJK_LEFT_BRACKET = re.compile(
    '([{CJK}])([\\(\\[\\{{<>\u201c])'.format(CJK=CJK))  # need to escape {
RIGHT_BRACKET_CJK = re.compile(
    '([\\)\\]\\}}<>\u201d])([{CJK}])'.format(CJK=CJK))  # need to escape }
FIX_LEFT_BRACKET_ANY_RIGHT_BRACKET = re.compile(
    r'([\(\[\{<\u201c]+)(\s*)(.+?)(\s*)([\)\]\}>\u201d]+)'
)  # need to escape { }
ANS_CJK_LEFT_BRACKET_ANY_RIGHT_BRACKET = re.compile(
    '([A-Za-z0-9{CJK}])[ ]*([\u201c])([A-Za-z0-9{CJK}\\-_ ]+)([\u201d])'.
    format(CJK=CJK))
LEFT_BRACKET_ANY_RIGHT_BRACKET_ANS_CJK = re.compile(
    '([\u201c])([A-Za-z0-9{CJK}\\-_ ]+)([\u201d])[ ]*([A-Za-z0-9{CJK}])'.
    format(CJK=CJK))

AN_LEFT_BRACKET = re.compile(r'([A-Za-z0-9])([\(\[\{])')
RIGHT_BRACKET_AN = re.compile(r'([\)\]\}])([A-Za-z0-9])')

CJK_ANS = re.compile(
        ('([{CJK}])([A-Za-z\u0370-\u03ff0-9@\\$%\\^&\\*\\-\\+\\\\=\\|'
         '/\u00a1-\u00ff\u2150-\u218f\u2700—\u27bf])')
        .format(CJK=CJK))
ANS_CJK = re.compile(
        ('([A-Za-z\u0370-\u03ff0-9~\\!\\$%\\^&\\*\\-\\+\\\\=\\|'
         ';:,\\./\\?\u00a1-\u00ff\u2150-\u218f\u2700—\u27bf])([{CJK}])')
        .format(CJK=CJK))

S_A = re.compile(r'(%)([A-Za-z])')

MIDDLE_DOT = re.compile(r'([ ]*)([\u00b7\u2022\u2027])([ ]*)')

# Python version only
TILDES = re.compile(r'~+')
EXCLAMATION_MARKS = re.compile(r'!+')
SEMICOLONS = re.compile(r';+')
COLONS = re.compile(r':+')
COMMAS = re.compile(r',+')
PERIODS = re.compile(r'\.+')
QUESTION_MARKS = re.compile(r'\?+')


def spacing(text):
    """
    Perform paranoid text spacing on text.
    """
    if len(text) <= 1 or not ANY_CJK.search(text):
        return text

    new_text = text

    new_text = DOTS_CJK.sub(r'\1 \2', new_text)
    new_text = FIX_CJK_COLON_ANS.sub(r'\1：\2', new_text)

    new_text = CJK_QUOTE.sub(r'\1 \2', new_text)
    new_text = QUOTE_CJK.sub(r'\1 \2', new_text)
    new_text = FIX_QUOTE_ANY_QUOTE.sub(r'\1\3\5', new_text)

    new_text = CJK_SINGLE_QUOTE_BUT_POSSESSIVE.sub(r'\1 \2', new_text)
    new_text = SINGLE_QUOTE_CJK.sub(r'\1 \2', new_text)
    new_text = FIX_POSSESSIVE_SINGLE_QUOTE.sub(r"\1's", new_text)

    new_text = HASH_ANS_CJK_HASH.sub(r'\1 \2\3\4 \5', new_text)
    new_text = CJK_HASH.sub(r'\1 \2', new_text)
    new_text = HASH_CJK.sub(r'\1 \3', new_text)

    new_text = CJK_OPERATOR_ANS.sub(r'\1 \2 \3', new_text)
    new_text = ANS_OPERATOR_CJK.sub(r'\1 \2 \3', new_text)

    new_text = FIX_SLASH_AS.sub(r'\1\2', new_text)
    new_text = FIX_SLASH_AS_SLASH.sub(r'\1\2\3', new_text)

    new_text = CJK_LEFT_BRACKET.sub(r'\1 \2', new_text)
    new_text = RIGHT_BRACKET_CJK.sub(r'\1 \2', new_text)
    new_text = FIX_LEFT_BRACKET_ANY_RIGHT_BRACKET.sub(r'\1\3\5', new_text)
    new_text = ANS_CJK_LEFT_BRACKET_ANY_RIGHT_BRACKET.sub(
        r'\1 \2\3\4', new_text)
    new_text = LEFT_BRACKET_ANY_RIGHT_BRACKET_ANS_CJK.sub(
        r'\1\2\3 \4', new_text)

    new_text = AN_LEFT_BRACKET.sub(r'\1 \2', new_text)
    new_text = RIGHT_BRACKET_AN.sub(r'\1 \2', new_text)

    new_text = CJK_ANS.sub(r'\1 \2', new_text)
    new_text = ANS_CJK.sub(r'\1 \2', new_text)

    new_text = S_A.sub(r'\1 \2', new_text)

    new_text = MIDDLE_DOT.sub('・', new_text)

    return new_text.strip()


def spacing_text(text):
    return spacing(text)


def pandoc_spacing(key, value, format, meta):
    if key == 'Str':
        return Str(spacing_text(value))


if __name__ == "__main__":
    toJSONFilter(pandoc_spacing)
