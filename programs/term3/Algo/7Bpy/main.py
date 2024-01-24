class Rib:
    def __init__(self, a, b, w):
        self.a = a
        self.b = b
        self.w = w

def find_root(x, parent):
    if parent[x] != x:
        parent[x] = find_root(parent[x], parent)
    return parent[x]

def merge_sets(x, y, parent, rank):
    root_x = find_root(x, parent)
    root_y = find_root(y, parent)

    if root_x != root_y:
        if rank[root_x] < rank[root_y]:
            parent[root_x] = root_y
        elif rank[root_x] > rank[root_y]:
            parent[root_y] = root_x
        else:
            parent[root_x] = root_y
            rank[root_y] += 1

n, m = map(int, input().split())
ribs = []

for _ in range(m):
    a, b, w = map(int, input().split())
    ribs.append(Rib(a - 1, b - 1, w))

ribs.sort(key=lambda x: x.w)

parent = list(range(n))
rank = [0] * n
total_weight = 0

for rib in ribs:
    a, b, w = rib.a, rib.b, rib.w
    if find_root(a, parent) != find_root(b, parent):
        total_weight += w
        merge_sets(a, b, parent, rank)

print(total_weight)

