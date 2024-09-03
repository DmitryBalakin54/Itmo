from collections import deque


def prufer_min(p: dict[int, int], v: list[int]) -> int:
    for ind, val in enumerate(v):
        if p[val] == 0:
            return ind


n = int(input())

prufer = deque([int(i) for i in input().split(' ')])
vertexes = [i + 1 for i in range(n)]
dct = {i: 0 for i in vertexes}
dct[0] = -1
for i in prufer:
    dct[i] += 1

while len(prufer) > 0:
    u = prufer[0]
    v = prufer_min(dct, vertexes)
    print(f'{vertexes[v]} {u}')
    prufer.popleft()
    dct[u] -= 1
    vertexes[v] = 0
vertexes.sort()
print(f'{vertexes[-1]} {vertexes[-2]}')
