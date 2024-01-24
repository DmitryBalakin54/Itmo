from collections import defaultdict, deque

def bfs_shortest_path(graph, start, end):
    queue = deque([(start, [start])])
    visited = set()

    while queue:
        current, path = queue.popleft()

        if current == end:
            return len(path) - 1, path

        if current not in visited:
            visited.add(current)

            for neighbor in graph[current]:
                queue.append((neighbor, path + [neighbor]))

    return -1, []

def main():
    n, m = map(int, input().split())
    edges = defaultdict(list)

    for _ in range(m):
        u, v = map(int, input().split())
        edges[u].append(v)

    start_vertex, end_vertex = map(int, input().split())

    path_length, path_nodes = bfs_shortest_path(edges, start_vertex, end_vertex)

    if path_length == -1:
        print(-1)
    else:
        print(path_length)
        print(*path_nodes)

if __name__ == "__main__":
    main()
