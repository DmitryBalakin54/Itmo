from collections import deque
from bisect import insort

n = int(input())
lst = [[] for _ in range(n + 1)]
deg = [0] * (n + 1)
graph = [set() for _ in range(n + 1)]

for _ in range(n-1):
    u, v = map(int, input().split())
    lst[u].append(v)
    lst[v].append(u)
    deg[u] += 1
    deg[v] += 1
    graph[u].add(v)
    graph[v].add(u)

leafs = deque(i for i in range(1, n + 1) if deg[i] == 1)

for _ in range(n - 2):
    leaf = leafs.popleft()
    neighbor = graph[leaf].pop()
    graph[neighbor].remove(leaf)
    deg[leaf] -= 1
    deg[neighbor] -= 1
    if deg[neighbor] == 1:
        insort(leafs, neighbor)
    print(neighbor, end=' ')
