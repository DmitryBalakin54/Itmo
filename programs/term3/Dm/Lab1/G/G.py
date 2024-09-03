from collections import deque


n, m = map(int, input().split(' '))

graph = [set() for i in range(n)]

for i in range(m):
    u, v = map(int, input().split(' '))
    graph[u - 1].add(v - 1)
    graph[v - 1].add(u - 1)

k = max(len(i) for i in graph)
k = k + 1 if k % 2 == 0 else k

print(k)

colors = [-1] * n
colors[0] = 0

vertexes = deque([0])

while len(vertexes) > 0:
    v = vertexes.popleft()
    for u in graph[v]:
        palette = [True] * k

        if colors[u] == -1:
            palette[colors[v]] = False

            for w in graph[u]:
                palette[colors[w]] = False

            colors[u] = -1

            for ind, col in enumerate(palette):
                if col:
                    colors[u] = ind
                    break

            vertexes.append(u)

for i in colors:
    print(i + 1)



