#include <iostream>
#include <vector>

using namespace std;

const int MAXN = 20005;

vector<int> adj[MAXN];
vector<bool> visited(MAXN, false);
vector<int> path;

void dfs(int v) {
    visited[v] = true;
    path.push_back(v);

    for (int u : adj[v]) {
        if (!visited[u]) {
            dfs(u);
        }
    }
}

int main() {
    int n, m;
    cin >> n >> m;

    for (int i = 0; i < m; ++i) {
        int a, b;
        cin >> a >> b;
        adj[a].push_back(b);
        adj[b].push_back(a);
    }

    vector<vector<int>> result;
    for (int i = 1; i <= n; ++i) {
        if (!visited[i]) {
            path.clear();
            dfs(i);
            result.push_back(path);
        }
    }

    cout << result.size() << endl;
    for (const auto& p : result) {
        cout << p.size() << " ";
        for (int v : p) {
            cout << v << " ";
        }
        cout << endl;
    }

    return 0;
}
