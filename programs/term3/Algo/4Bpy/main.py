from collections import defaultdict

def find_eulerian_path(graph, start_node):
    stack = [start_node]
    path = []

    while stack:
        current_node = stack[-1]

        if graph[current_node]:
            next_node = graph[current_node].pop()
            stack.append(next_node)
        else:
            path.append(stack.pop())

    return path[::-1]

def main():
    n = int(input())
    substrings = [input() for _ in range(n)]

    out_degree = defaultdict(int)
    in_degree = defaultdict(int)
    graph = defaultdict(list)

    for substring in substrings:
        out_degree[substring[:2]] += 1
        in_degree[substring[1:]] += 1
        graph[substring[:2]].append(substring[1:])

    start_node = None
    end_node = None

    for node in set(in_degree.keys()) | set(out_degree.keys()):
        if in_degree[node] == out_degree[node] + 1:
            if end_node is not None:
                print("NO")
                return
            end_node = node
        elif out_degree[node] == in_degree[node] + 1:
            if start_node is not None:
                print("NO")
                return
            start_node = node
        elif out_degree[node] != in_degree[node]:
            print("NO")
            return

    start_node = start_node or list(out_degree.keys())[0]
    eulerian_path = find_eulerian_path(graph, start_node)

    if len(eulerian_path) != n + 1:
        print("NO")
    else:
        print("YES")
        print("".join(eulerian_path[:-1]))

if __name__ == "__main__":
    main()
