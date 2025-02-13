import math
from collections import Counter, defaultdict

kx, ky = map(int, input().split())
n = int(input())
data = [tuple(map(int, input().split())) for _ in range(n)]

px = Counter(x for x, _ in data)
py_given_x = defaultdict(Counter)

for x, y in data:
    py_given_x[x][y] += 1

hx_y = 0.0
for x, x_count in px.items():
    px_prob = x_count / n
    hy_given_x = 0.0

    total_y_for_x = sum(py_given_x[x].values())
    for y, count_yx in py_given_x[x].items():
        pyx_prob = count_yx / total_y_for_x
        hy_given_x -= pyx_prob * math.log(pyx_prob)

    hx_y += px_prob * hy_given_x

print(hx_y)
