import math


class Point:
    def __init__(self, x, y):
        self.x = x
        self.y = y


dist = (lambda v1, v2: (v1.x - v2.x) ** 2 + (v1.y - v2.y) ** 2)


n = int(input())
vertex = []

for _ in range(n):
    x, y = map(int, input().split())
    vertex.append(Point(x, y))

used = [False] * n
min_dist = [float('inf')] * n
end = [-1] * n
min_dist[0] = 0

ans = 0
for _ in range(n):
    u = -1
    for j in range(n):
        if not used[j] and (u == -1 or min_dist[j] < min_dist[u]):
            u = j
    used[u] = True
    ans += 0 if u == 0 else math.sqrt(dist(vertex[u], vertex[end[u]]))
    for v in range(n):
        if dist(vertex[u], vertex[v]) < min_dist[v]:
            min_dist[v] = dist(vertex[u], vertex[v])
            end[v] = u

print('{:.11f}'.format(ans))
