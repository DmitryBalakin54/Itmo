from collections import deque

n = int(input())
graph = [[False for _ in range(n)] for _ in range(n)]

for i in range(n):
    s = input()
    for j in range(len(s)):
        if s[j] == '1':
            graph[i][j] = True
            graph[j][i] = True

deq = deque(i for i in range(n))

for i in range(n * 6):
    if not graph[deq[0]][deq[1]]:
        inv = 2
        while inv < len(deq) - 1 and not (graph[deq[0]][deq[inv]] and graph[deq[1]][deq[inv + 1]]):
            inv += 1
        if inv == len(deq) - 1:
            inv = 2
            while not graph[deq[0]][deq[inv]]:
                inv += 1
        for j in range(1, (inv + 2) // 2):
            ind = inv + 1 - j
            tmp = deq[ind]
            deq[ind] = deq[j]
            deq[j] = tmp
    deq.append(deq.popleft())


print(' '.join(str(i + 1) for i in deq))
