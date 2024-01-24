def dfs(graph, start, visited, path):
    visited[start] = True
    path.append(start)

    for neighbor in graph[start]:
        if not visited[neighbor]:
            dfs(graph, neighbor, visited, path)

def find_minimal_paths(n, edges):
    graph = {i: [] for i in range(1, n + 1)}

    for edge in edges:
        a, b = edge
        graph[a].append(b)
        graph[b].append(a)

    visited = {i: False for i in range(1, n + 1)}
    paths = []

    for vertex in range(1, n + 1):
        if not visited[vertex]:
            path = []
            dfs(graph, vertex, visited, path)
            paths.append(path)

    return paths

if __name__ == "__main__":
    n, m = map(int, input().split())
    edges = [tuple(map(int, input().split())) for _ in range(m)]

    paths = find_minimal_paths(n, edges)

    print(len(paths))
    for path in paths:
        print(len(path), *path)
