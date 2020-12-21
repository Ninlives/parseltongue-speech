from pandocfilters import toJSONFilter


def extract_code(key, value, format, meta):
    if key == 'CodeBlock':
        print(value)


if __name__ == "__main__":
    toJSONFilter(extract_code)
