import random


def dfs(g: list[dict[int, bool]], u: int, mark: list[bool], t: list[int]) -> None:
    mark[u] = True
    for v in g[u]:
        if not mark[v]:
            dfs(g, v, mark, t)
    t.append(u)


n = int(input())
graph = [dict() for _ in range(n)]

for i in range(n):
    s = input()
    for j in range(len(s)):
        if s[j] == '1':
            graph[i][j] = True
        else:
            graph[j][i] = True

top = []
lst = [i for i in range(n)]
random.shuffle(lst)
for i in lst:
    dfs(graph, i, [False] * n, top)
    if graph[top[0]].get(top[-1]):
        print(' '.join(str(top[ind] + 1) for ind in range(len(top) - 1, -1, -1)))
        break
    top = []
