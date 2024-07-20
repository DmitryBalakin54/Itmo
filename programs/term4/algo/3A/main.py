from collections import deque, defaultdict


def bfs_capacity(graph, source, sink, parent):
    visited = set()
    queue = deque([source])
    visited.add(source)

    while queue:
        u = queue.popleft()

        for v in graph[u]:
            if v not in visited and graph[u][v] > 0:  
                queue.append(v)
                visited.add(v)
                parent[v] = u
                if v == sink:
                    return True
    return False


def edmonds_karp(graph, source, sink):
    parent = {}
    max_flow = 0

    while bfs_capacity(graph, source, sink, parent):
        path_flow = float('Inf')
        s = sink

        while s != source:
            path_flow = min(path_flow, graph[parent[s]][s])
            s = parent[s]

        v = sink
        while v != source:
            u = parent[v]
            graph[u][v] -= path_flow
            graph[v][u] += path_flow
            v = parent[v]

        max_flow += path_flow

    return max_flow


def find_max_flow(n, edges):
    graph = defaultdict(lambda: defaultdict(int))
    for a, b, c in edges:
        graph[a][b] = c
        if b not in graph:
            graph[b] = defaultdict(int)

    source = 1
    sink = n
    return edmonds_karp(graph, source, sink)


n, m = map(int, input().split())
edges = [tuple(map(int, input().split())) for _ in range(m)]

print(find_max_flow(n, edges))
