#include <iostream>
#include <set>
#include <vector>

using namespace std;

int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    int n;
    cin >> n;

    vector<int> prufer(n - 2);
    vector<int> deg(n + 1, 1);

    for (int i = 0; i < n - 2; i++) {
        cin >> prufer[i];
        deg[prufer[i]]++;
    }

    set<int> leafs;

    for (int i = 0; i < n; i++) {
        if (deg[i + 1] == 1) {
            leafs.insert(i + 1);
        }
    }

    int prufer_begin = 0;
    while (prufer_begin < prufer.size()) {
        int leaf = *leafs.begin();
        int v = prufer[prufer_begin++];

        cout << leaf << ' ' << v << '\n';

        leafs.erase(leafs.begin());
        deg[v]--;

        if (deg[v] == 1) {
            leafs.insert(v);
        }
    }
    cout << *leafs.begin() << ' ' << *--leafs.end() << '\n';

    return 0;
}