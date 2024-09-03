def build(s):
    s += "#"
    n = len(s)
    p = [0] * n
    c = [0] * n
    a = [(s[i], i) for i in range(n)]
    a.sort(key=lambda x: x[0])
    for i in range(n):
        p[i] = a[i][1]

    for i in range(1, n):
        c[p[i]] = c[p[i - 1]]
        if a[i][0] != a[i - 1][0]:
            c[p[i]] += 1

    cc = [0] * n
    k = 0
    while (1 << k) < n:
        for i in range(n):
            p[i] = (p[i] - (1 << k) + n) % n
        p = cnt(p, c)
        for i in range(n):
            cc[i] = 0

        for i in range(1, n):
            cc[p[i]] = cc[p[i - 1]]
            if c[p[i - 1]] != c[p[i]] or c[(p[i - 1] + (1 << k)) % n] != c[(p[i] + (1 << k)) % n]:
                cc[p[i]] += 1

        c[:] = cc[:]
        k += 1

    return p[1:]


def cnt(p, c):
    n = len(p)
    cnt = [0] * n
    for i in c:
        cnt[i] += 1

    pp = [0] * n
    pos = [0] * n
    pos[0] = 0

    for i in range(1, n):
        pos[i] = pos[i - 1] + cnt[i - 1]

    for i in p:
        k = c[i]
        pp[pos[k]] = i
        pos[k] += 1

    return pp
print(*[i + 1 for i in build(input().strip())])
