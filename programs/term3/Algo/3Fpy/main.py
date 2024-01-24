def find(graph):
    n = len(graph)
    d = [-1] * n
    l = [-1] * n
    was = [False] * n
    stack = []
    res = []
    time = 0

    def dfs(u, parent):
        nonlocal time
        d[u] = time
        l[u] = time
        time += 1
        was[u] = True
        cld = 0

        for vert in graph[u]:
            if not was[vert]:
                cld += 1
                stack.append((u, vert))
                dfs(vert, u)
                l[u] = min(l[u], l[vert])

                if l[vert] >= d[u]:
                    component = []
                    while stack[-1] != (u, vert):
                        component.append(stack.pop())
                    component.append(stack.pop())
                    res.append(component)
            elif vert != parent and d[vert] < l[u]:
                l[u] = d[vert]
                stack.append((u, vert))

    for i in range(n):
        if not was[i]:
            dfs(i, -1)

    return res


def main():
    n, m = map(int, input().split())
    graph = [[] for _ in range(n)]

    for _ in range(m):
        u, v = map(int, input().split())
        graph[u - 1].append(v - 1)
        graph[v - 1].append(u - 1)

    comps = find(graph)

    comp_n = {edge: idx + 1 for idx, component in enumerate(comps) for edge in component}

    print(len(comps))
    print(" ".join(str(comp_n[edge]) for comp in comps for edge in comp))


if __name__ == "__main__":
    main()
