import sys


def parse(_line):
    global line, pos
    line = _line + '#'
    pos = 0

    def skip(s):
        global line, pos
        if line.startswith(s, pos):
            pos += len(s)
            return True
        return False

    def e():
        x = dij()
        if skip('->'):
            x = ['->', x, e()]
        return x

    def dij():
        x = con()
        while skip('|'):
            x = ['|', x, con()]
        return x

    def con():
        x = nt()
        while skip('&'):
            x = ['&', x, nt()]
        return x

    def nt():
        global pos, line
        if skip('('):
            x = e()
            skip(')')
            return x
        if skip('!'):
            return ['!', nt()]
        x = ''
        while line[pos].isdigit() or line[pos].isalpha() or line[pos] == chr(39):
            x += line[pos]
            pos += 1
        return x

    return e()


def make_expr_str(expr):
    res = ''
    for i in expr:
        if type(i) == list:
            if i[0] == '!':
                res += '(' + i[0] + make_expr_str([i[1]]) + ')'
            else:
                res += '(' + i[0] + ',' + make_expr_str([i[1]]) + ',' + make_expr_str([i[2]]) + ')'
        else:
            res += i

    return res


def main():
    sys.setrecursionlimit(2 ** 30)
    line = ''

    with sys.stdin as f:
        for x in f.readline():
            if not x.isspace():
                line += x

    e = parse(line)
    print(make_expr_str([e]))


if __name__ == '__main__':
    main()
