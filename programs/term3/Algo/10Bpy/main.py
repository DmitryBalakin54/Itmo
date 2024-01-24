def ans(s, k):
    all = 256
    n = len(s)
    cs = [0] * n
    pm = [0] * n
    cnt = [0] * max(n, all)

    for i in range(n):
        cnt[ord(s[i])] += 1

    for i in range(1, all):
        cnt[i] += cnt[i - 1]

    for i in range(n - 1, -1, -1):
        cnt[ord(s[i])] -= 1
        pm[cnt[ord(s[i])]] = i

    cs[pm[0]] = 0
    num_cl = 1
    for i in range(1, n):
        if s[pm[i]] != s[pm[i - 1]]:
            num_cl += 1
        cs[pm[i]] = num_cl - 1

    npm = [0] * n
    ncs = [0] * n
    j = 0
    while (1 << j) < n:
        length = 1 << j
        for i in range(n):
            npm[i] = pm[i] - length
            if npm[i] < 0:
                npm[i] += n

        cnt = [0] * num_cl
        for i in range(n):
            cnt[cs[npm[i]]] += 1

        for i in range(1, num_cl):
            cnt[i] += cnt[i - 1]

        for i in range(n - 1, -1, -1):
            cnt[cs[npm[i]]] -= 1
            pm[cnt[cs[npm[i]]]] = npm[i]

        ncs[pm[0]] = 0
        num_cl = 1
        for i in range(1, n):
            cur = (cs[pm[i]], cs[(pm[i] + (1 << j)) % n])
            prev = (cs[pm[i - 1]], cs[(pm[i - 1] + (1 << j)) % n])
            if cur != prev:
                num_cl += 1
            ncs[pm[i]] = num_cl - 1

        cs, ncs = ncs, cs
        j += 1

    was = [False] * n
    cur = -1

    if k >= n:
        return "IMPOSSIBLE"

    for i in range(n):
        if not was[cs[pm[i]]]:
            was[cs[pm[i]]] = True
            cur += 1

        if not cur != k:
            res = s[pm[i]:] + s[:pm[i]]
            return res

    return "IMPOSSIBLE"


print(ans(input(), int(input()) - 1))
