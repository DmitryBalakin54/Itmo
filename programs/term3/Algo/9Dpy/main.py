def f(s, sss):
    cnt = 0
    for ss in sss:
        if ss < s:
            cnt += 1
    return cnt

def g(ss, s, mods):
    res = [f(s, ss)]

    for ki, ci in mods:
        ki = int(ki) - 1
        s = s[:ki] + ci * (len(s) - ki)
        res.append(f(s, ss))

    return res


n, q = map(int, input().split())
init = input().strip()
d = [input().strip() for _ in range(n)]
mods = [list(map(str, input().split())) for _ in range(q)]

for ans in g(d, init, mods):
    print(ans)
