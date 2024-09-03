def get(el):
    if p[el] == el:
        return el
    p[el] = get(p[el])
    return p[el]

with open('destroy.in', 'r') as inf:
    with open('destroy.out', 'w') as outf:

        n, m, s = map(int, inf.readline().split())
        val = [1] * n
        p = list(range(n))

        edges = []
        for i in range(m):
            a, b, c = map(int, inf.readline().split())
            edges.append(((c, i + 1), (a - 1, b - 1)))

        edges.sort(key=lambda x: x[0][0], reverse=True)

        unus = []
        res = []
        cost = 0

        for i in range(m):
            if get(edges[i][1][0]) == get(edges[i][1][1]):
                unus.append(i)
            else:
                a = get(edges[i][1][0])
                b = get(edges[i][1][1])
                if a != b:
                    if val[a] > val[b]:
                        a, b = b, a
                    p[a] = b
                    val[b] += val[a]


        for i in reversed(unus):
            if cost + edges[i][0][0] <= s:
                res.append(edges[i][0][1])
                cost += edges[i][0][0]

        res.sort()
        outf.write(str(len(res)) + '\n')
        outf.write(' '.join(str(i) for i in res))

