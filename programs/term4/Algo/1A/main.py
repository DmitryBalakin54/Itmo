def kuhn(u, used, match, graph):
    if used[u]:
        return False
    used[u] = True
    for v in graph[u]:
        if match[v] == -1 or kuhn(match[v], used, match, graph):
            match[v] = u
            return True
    return False


def maximum_matching(n, m, edges):
    graph = [[] for _ in range(n)]
    for i, edge_list in enumerate(edges):
        for v in edge_list:
            graph[i].append(v - 1)

    match = [-1] * m
    result = []

    for i in range(n):
        used = [False] * n
        kuhn(i, used, match, graph)

    for v in range(m):
        if match[v] != -1:
            result.append((match[v] + 1, v + 1))

    return len(result), result


data = input().split()

n = int(data[0])
m = int(data[1])

edges = []
index = 0

for _ in range(n):
    data = input().split()
    edge_list = []
    while int(data[index]) != 0:
        edge_list.append(int(data[index]))
        index += 1
    index = 0
    edges.append(edge_list)

l, matching = maximum_matching(n, m, edges)

print(l)
for u, v in matching:
    print(u, v)
