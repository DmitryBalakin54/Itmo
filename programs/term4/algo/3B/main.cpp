#include <iostream>
#include <vector>
#include <queue>
#include <cstring>
#include <algorithm>

using namespace std;

const int MAXN = 1000;
const int INF = 1e9;

struct Edge {
    int v, c, f;
    Edge* back;
    Edge(int v, int c) : v(v), c(c), f(0), back(nullptr) {}
};

vector<Edge*> graph[MAXN];
int d[MAXN];
int edge[MAXN];

int dfs(int u, int t, int f, int k) {
    if (u == t) return f;
    for (int& i = edge[u]; i < graph[u].size(); i++) {
        Edge* e = graph[u][i];
        if (d[e->v] == d[u] + 1 && (long long)e->f + (1 << k) <= e->c) {
            int delta = dfs(e->v, t, min(f, e->c - e->f), k);
            if (delta > 0) {
                e->f += delta;
                e->back->f -= delta;
                return delta;
            }
        }
    }
    return 0;
}

int bfs(int s, int t, int k) {
    memset(d, -1, sizeof(d));
    d[s] = 0;
    queue<int> q;
    q.push(s);
    while (!q.empty()) {
        int u = q.front();
        q.pop();
        for (Edge* e : graph[u]) {
            if (d[e->v] < 0 && (long long)e->f + (1 << k) <= e->c) {
                d[e->v] = d[u] + 1;
                q.push(e->v);
            }
        }
    }
    return d[t];
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, m;
    cin >> n >> m;

    for (int i = 0; i < m; i++) {
        int a, b, c;
        cin >> a >> b >> c;
        a--; b--;
        Edge* f = new Edge(b, c);
        Edge* s = new Edge(a, 0);
        f->back = s;
        s->back = f;
        graph[a].push_back(f);
        graph[b].push_back(s);
    }

    long long maxFlow = 0;
    for (int k = 30; k >= 0; k--) {
        while (bfs(0, n - 1, k) >= 0) {
            memset(edge, 0, sizeof(edge));
            while (true) {
                int flow = dfs(0, n - 1, INF, k);
                if (flow == 0) break;
                maxFlow += flow;
            }
        }
    }

    cout << maxFlow << "\n";

    return 0;
}
