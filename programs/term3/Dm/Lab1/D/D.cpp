#include<iostream>
#include<vector>

using namespace std;

void dfs(vector<vector<bool>> &g, int u, vector<bool> &mark, vector<int> &t) {
    mark[u] = true;
    for (int i = 0; i < g[u].size(); i++) {
        if (!mark[i] && g[u][i]) {
            dfs(g, i, mark, t);
        }
    }
    t.push_back(u);
}

int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    int n;
    cin >> n;
    vector<vector<bool>> graph(n, vector<bool>(n, false));

    for (int i = 1; i < n; i++) {
        string s;
        cin >> s;
        for (int j = 0; j < s.length(); j++) {
            if (s[j] == '1') {
                graph[i][j] = true;
            } else {
                graph[j][i] = true;
            }
        }
    }

    vector<int> top;
    for (int i = 0; i < n; i++) {
        vector<bool> mark(n, false);
        dfs(graph, i, mark, top);
        if (graph[top[0]][top[top.size() - 1]]) {
            for (int j = top.size() - 1; j >= 0; j--) {
                cout << top[j] + 1<< ' ';
            }
            break;
        }
        top.clear();
    }
    return 0;
}