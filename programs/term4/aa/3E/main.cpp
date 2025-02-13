#include <iostream>
#include <vector>
#include <queue>
#include <cstring>
#include <algorithm>
#include <sstream>

using namespace std;

const int MAXN = 1000;
const int INF = 1e9;

struct Edge {
    int from, to, c, f, index;
    Edge* back;
    Edge(int u, int v, int c, int idx) : from(u), to(v), c(c), f(0), index(idx), back(nullptr) {}
};

vector<Edge*> graph[MAXN];
int d[MAXN];
int edge[MAXN];

int dfs(int u, int t, int f, int k) {
    if (u == t) return f;
    for (int& i = edge[u]; i < graph[u].size(); i++) {
        Edge* e = graph[u][i];
        if (d[e->to] == d[u] + 1 && (long long)e->f + (1 << k) <= e->c) {
            int delta = dfs(e->to, t, min(f, e->c - e->f), k);
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
            if (d[e->to] < 0 && (long long)e->f + (1 << k) <= e->c) {
                d[e->to] = d[u] + 1;
                q.push(e->to);
            }
        }
    }
    return d[t];
}

struct Path {
    vector<Edge*> edges;
    int cap;
    Path(int cap) : cap(cap) {}
};

Path* dfsPath(int v, int t, int flow, vector<bool>& used) {
    if (v == t) return new Path(flow);
    used[v] = true;
    for (Edge* e : graph[v]) {
        if (!used[e->to] && e->f > 0) {
            Path* result = dfsPath(e->to, t, min(flow, e->f), used);
            if (result) {
                e->f -= result->cap;
                result->edges.push_back(e);
                return result;
            }
        }
    }
    return nullptr;
}

string extractPath(int n) {
    vector<bool> used(n, false);
    Path* path = dfsPath(0, n - 1, INF, used);
    if (!path) return "";

    stringstream res;
    res << path->cap << " " << path->edges.size();
    reverse(path->edges.begin(), path->edges.end());
    for (Edge* edge : path->edges) {
        res << " " << edge->index + 1;
    }

    delete path;
    return res.str();
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, m;
    cin >> n >> m;

    for (int i = 0; i < m; i++) {
        int a, b, c;
        cin >> a >> b >> c;
        a--; b--; // Convert to zero-based indexing

        Edge* f = new Edge(a, b, c, i);
        Edge* s = new Edge(b, a, 0, -1);
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

    vector<string> decomposition;
    string path = extractPath(n);
    while (!path.empty()) {
        decomposition.push_back(path);
        path = extractPath(n);
    }

    cout << decomposition.size() << "\n";
    for (const string& result : decomposition) {
        cout << result << "\n";
    }

    return 0;
}
