class N:
    def __init__(self):
        self.n = [None] * 30
        self.l = [None] * 30
        self.t = False
        self.lf = -1
        self.p = None
        self.sl = None
        self.nsl = None
        self.nb = []
        self.c = 0


def gi(ch):
    return ord(ch) - ord('a')


def as_(st, nb):
    c = r
    for ch in st:
        i = gi(ch)
        if not c.n[i]:
            c.n[i] = N()
            c.n[i].p = c
            c.n[i].lf = ch
        c = c.n[i]
    c.t = True
    c.nb.append(nb)


def g(c):
    if not c.sl:
        if c == r or c.p == r:
            c.sl = r
        else:
            c.sl = nx(g(c.p), c.lf)
            if c.sl.t:
                c.nsl = c.sl
            else:
                c.nsl = c.sl.nsl
    return c.sl


def nx(c, ch):
    i = gi(ch)
    if not c.l[i]:
        if c.n[i]:
            c.l[i] = c.n[i]
        else:
            if c == r:
                c.l[i] = r
            else:
                c.l[i] = nx(g(c), ch)
    g(c.l[i])
    return c.l[i]


def df(v):
    wa[id(v)] = True
    for to in v.l:
        if to and not wa.get(id(to), False):
            te = to
            co = to.c
            if co:
                while te and te != r:
                    if te.t:
                        for nu in te.nb:
                            an[nu] += co
                    te = te.nsl
                df(to)


st = []
t = ""
s = ""
n = 0
m = 0
f = 0
ans_s = ""
MAX_N = 1000000 + 50

an = [0] * (MAX_N)

r = N()

n = int(input())
for i in range(n):
    s = input().strip()
    as_(s, i)

an = [0] * n
t = input().strip()

wa = {}

cu = r
for i in range(len(t)):
    cu = nx(cu, t[i])
    cu.c += 1
df(r)

for i in range(n):
    print(an[i])
