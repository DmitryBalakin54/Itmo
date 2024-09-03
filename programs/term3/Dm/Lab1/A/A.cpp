#include<iostream>
#include<vector>
#include<deque>

using namespace std;

int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);
    cout.tie(0);

    int n;
    cin >> n;
    vector<vector<bool>> g(n, vector<bool>(n, false));

    for (int i = 0; i < n; i++) {
        string s;
        cin >> s;
        for (int j = 0; j < s.length(); j++) {
            if (s[j] == '1') {
                g[j][i] = true;
                g[i][j] = true;
            }
        }
    }

    deque<int> deq;
    for (int i = 0; i < n; i++) {
        deq.push_back(i);
    }

    for (int i = 0; i < n * n; i++) {
        if (!g[deq[0]][deq[1]]) {
            int k = 2;
            while (!(g[deq[0]][deq[k]] && g[deq[1]][deq[k + 1]])) {
                k++;
            }
            for (int j = 1; j < (k + 2) / 2; j++) {
                int ind = k + 1 - j;
                int tmp = deq[ind];
                deq[ind] = deq[j];
                deq[j] = tmp;
            }
        }
        deq.push_back(deq.front());
        deq.pop_front();
    }

    for (int i = 0; i < n; i++) {
        cout << deq[i] + 1 << ' ';
    }

    return 0;
}