#include <iostream>
#include <vector>
#include <queue>

using namespace std;

const int inf = 1000000000;

struct Vertex {
    int x, y;
};


int find(const vector<int>& X, const vector<int>& Y,  int s, int t) {
    int n = (int) X.size();
    priority_queue<pair<int, int>, vector<pair<int, int>>, greater<>> q;
    vector<int> rho(n, inf);


    q.emplace(0, s - 1);
    rho[s - 1] = 0;
    int res = rho[t - 1];
    int time = 0;

    while (!q.empty()) {
        time++;
        if (time == 800000) {
            return rho[t  -1];
        }

        int cur_rho = q.top().first;
        int u = q.top().second;
        q.pop();

        if (cur_rho > rho[u]) {
            continue;
        }

        for (int v = 0; v < n; v++) {
            if (u == v) {
                continue;
            }

            int t1 = X[u] - X[v];
            int t2 = Y[u] - Y[v];
            int time = t1 * t1 + t2 * t2;
            if (rho[u] + time < rho[v]) {
                rho[v] = rho[u] + time;
                if (v == t - 1) {
                    time = 0;
                }
                q.emplace(rho[v], v);
            }
        }
    }

    return rho[t - 1];
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(0);
    cout.tie(0);
    int n;
    cin >> n;

    vector<int> X(n);
    vector<int> Y(n);
    for (int i = 0; i < n; i++) {
        cin >> X[i] >> Y[i];
    }

    int s, t;
    cin >> s >> t;

    int res = find(X, Y, s, t);

    cout << res << "\n";

    return 0;
}
