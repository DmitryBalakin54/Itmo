#include <iostream>
#include <vector>
#include <set>

using namespace std;

void dfs(vector<int>* graph, int vert_u, vector<bool>& was, int& time, vector<int>& t, vector<int>& up, vector<int>& p, set<int>& res) {
    time++;
    t[vert_u] = time;
    up[vert_u] = time;
    was[vert_u] = true;
    int var = 0;
    for (int vertex : graph[vert_u]) {
        if (!was[vertex]) {
            var++;
            p[vertex] = vert_u;
            dfs(graph, vertex, was, time, t, up, p, res);
            up[vert_u] = min(up[vert_u], up[vertex]);
            if (p[vert_u] != -1 && up[vertex] >= t[vert_u]) {
                res.insert(vert_u + 1);
            }
            if (p[vert_u] == -1 && var > 1) {
                res.insert(vert_u + 1);
            }
        } else if (vertex != p[vert_u]) {
            up[vert_u] = min(up[vert_u], t[vertex]);
        }
    }
}

int main() {
    int n, m;
    cin >> n >> m;

    auto* graph = new vector<int>[n];
    for (int i = 0; i < n; i++) {
        graph[i] = vector<int>();
    }

    for (int i = 0; i < m; i++) {
        int a, b;
        cin >> a >> b;
        graph[a - 1].push_back(b - 1);
        graph[b - 1].push_back(a - 1);
    }

    vector<bool> was(n, false);
    vector<int> t(n);
    vector<int> up(n);
    vector<int> p(n, -1);

    int timer = 0;
    set<int> res;
    for (int i = 0; i < n; i++) {
        if (!was[i]) {
            dfs(graph, i, was, timer, t, up, p, res);
        }
    }

    cout << res.size() << endl;
    for (int edge : res) {
        cout << edge << " ";
    }
    cout << endl;
    delete[] graph;

    return 0;
}
