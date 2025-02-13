def dfs(u, g, used, a):
    used[u] = True
    for v in g[u]:
        if a[v] == -1:
            a[v] = u
            return True
        if not used[a[v]] and dfs(a[v], g, used, a):
            a[v] = u
            return True
    return False


class DSU:
    def __init__(self, n):
        self.parent = list(range(n))
        self.rank = [0] * n
        self.comps = n

    def find(self, x):
        if self.parent[x] != x:
            self.parent[x] = self.find(self.parent[x])
        return self.parent[x]

    def union(self, x, y):
        rootX = self.find(x)
        rootY = self.find(y)
        if rootX != rootY:
            self.comps -= 1
            if self.rank[rootX] == self.rank[rootY]:
                self.rank[rootX] += 1
            if self.rank[rootX] <= self.rank[rootY]:
                self.parent[rootX] = rootY
            else:
                self.parent[rootY] = rootX

    def get_comps(self):
        return self.comps


def main():
    n, k = map(int, input().split())

    g = [[] for _ in range(n)]
    for _ in range(k):
        a, b = map(int, input().split())
        g[a - 1].append(b - 1)

    matching = [-1] * n
    for i in range(n):
        used = [False] * n
        dfs(i, g, used, matching)

    dsu = DSU(n)
    for i in range(n):
        if matching[i] != -1:
            dsu.union(i, matching[i])

    print(dsu.get_comps())


if __name__ == "__main__":
    main()
