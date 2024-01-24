#include <iostream>
#include <vector>

using namespace std;

const int inf = 30000;

int main() {
    ios::sync_with_stdio(false);
    cin.tie(0);
    cout.tie(0);

    int n, m;
    cin >> n >> m;

    vector<vector<int>> graph(n, vector<int>(n, inf));

    for (int i = 0; i < m; i++) {
        int s, t, w;
        cin >> s >> t >> w;
        graph[s - 1][t - 1] = min(graph[s - 1][t - 1], w);
    }

    for (int k = 0; k < n; k++) {
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                if (graph[i][k] < inf && graph[k][j] < inf) {
                    graph[i][j] = min(graph[i][j], graph[i][k] + graph[k][j]);
                }
            }
        }
    }

    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            if (i == j) {
                cout << "0 ";
            } else if (graph[i][j] == inf) {
                cout << "30000 ";
            } else {
                cout << graph[i][j] << " ";
            }
        }
        cout << "\n";
    }

    return 0;
}