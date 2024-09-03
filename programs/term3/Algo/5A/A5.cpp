#include <iostream>
#include <vector>
#include <queue>

using namespace std;

void bfs_shortest_path(vector<vector<int>>& graph, int start, int end, int n) {
    queue<int> q;
    vector<bool> used (n);
    vector<int> d (n);
    vector<int> p (n);
    p[start] = -1;
    used[start] = true;
    q.push (start);
    while (!q.empty()) {
        int v = q.front();
        q.pop();
        for (int i=0; i < graph[v].size(); ++i) {
            int to = graph[v][i];
            if (!used[to]) {
                used[to] = true;
                p[to] = v;
                d[to] = d[v] + 1;
                q.push (to);
            }
        }
    }

    if (!used[end])
        cout << -1;
    else {
        vector<int> path;
        for (int v = end; v != -1; v = p[v])
            path.push_back(v);
        cout << path.size() - 1 << "\n";
        for (int i = path.size() - 1; i >= 0; i--)
            cout << path[i] + 1<< " ";
    }
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(0);
    cout.tie(0);

    int n, m;
    cin >> n >> m;

    vector<vector<int>> edges(n);

    for (int i = 0; i < m; i++) {
        int u, v;
        cin >> u >> v;
        edges[u - 1].push_back(v - 1);
    }

    int start_vertex, end_vertex;
    cin >> start_vertex >> end_vertex;

    bfs_shortest_path(edges, start_vertex - 1, end_vertex - 1, n);
    return 0;
}
