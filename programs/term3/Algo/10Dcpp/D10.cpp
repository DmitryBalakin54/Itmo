#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

class DSU {
public:
    vector<int> parent, size, start;

    DSU(int n) {
        parent.resize(n);
        size.resize(n, 1);
        start.resize(n);
        for (int i = 0; i < n; i++) {
            parent[i] = i;
            start[i] = i;
        }
    }

    int find(int index) {
        if (parent[index] != index) {
            parent[index] = find(parent[index]);
        }
        return parent[index];
    }

    void unionSets(int x, int y) {
        x = find(x);
        y = find(y);
        if (x != y) {
            size[y] += size[x];
            start[y] = min(start[x], start[y]);
            parent[x] = y;
        }
    }

    int getSize(int place) {
        return size[find(place)];
    }

    int getSize() {
        return parent.size();
    }

    int getStart(int x) {
        return start[find(x)];
    }
};

pair<vector<int>, vector<int>> buildSufArray(const string& s) {
    string input = s + "#";
    int n = input.length();
    vector<int> p(n), c(n), cc(n);

    vector<pair<char, int>> a(n);
    for (int i = 0; i < n; i++) {
        a[i] = {input[i], i};
    }
    sort(a.begin(), a.end(), [](const auto& p1, const auto& p2) {
        return p1.first < p2.first;
    });

    for (int i = 0; i < n; i++) {
        p[i] = a[i].second;
    }

    for (int i = 1; i < n; i++) {
        c[p[i]] = c[p[i - 1]];
        if (a[i].first != a[i - 1].first) {
            c[p[i]]++;
        }
    }

    int k = 0;
    while ((1 << k) < n) {
        for (int i = 0; i < n; i++) {
            p[i] = (p[i] - (1 << k) + n) % n;
        }
        sort(p.begin(), p.end(), [&](int i, int j) {
            return make_pair(c[i], c[(i + (1 << k)) % n]) < make_pair(c[j], c[(j + (1 << k)) % n]);
        });

        for (int i = 0; i < n; i++) {
            cc[i] = 0;
        }

        for (int i = 1; i < n; i++) {
            cc[p[i]] = cc[p[i - 1]];
            if (c[p[i - 1]] != c[p[i]] || c[(p[i - 1] + (1 << k)) % n] != c[(p[i] + (1 << k)) % n]) {
                cc[p[i]]++;
            }
        }

        copy(cc.begin(), cc.end(), c.begin());
        k++;
    }

    vector<int> lcp(n);
    k = 0;
    for (int i = 0; i < n - 1; i++) {
        int pIndex = c[i];
        int j = p[pIndex - 1];
        while (input[i + k] == input[j + k]) {
            k++;
        }
        lcp[pIndex] = k;
        k = max(k - 1, 0);
    }

    vector<int> resLcp(lcp.begin() + 1, lcp.end());
    vector<int> resP(p.begin() + 1, p.end());

    return make_pair(resP, resLcp);
}

void solve() {
    int n, m;
    cin >> n >> m;

    vector<int> a(n);
    for (int i = 0; i < n; i++) {
        cin >> a[i];
        a[i]--;
    }

    string s;
    for (int i = 0; i < n; i++) {
        s += to_string(a[i]);
    }

    auto pair = buildSufArray(s);
    vector<int> suf = pair.first;
    vector<int> lcp = pair.second;

    vector<::pair<int, int>> indexedLcp(lcp.size() - 1);
    for (int i = 1; i < lcp.size(); i++) {
        indexedLcp[i - 1] = {i - 1, lcp[i]};
    }

    sort(indexedLcp.begin(), indexedLcp.end(), [](const auto& p1, const auto& p2) {
        return p1.second > p2.second;
    });

    long long maxVal = n;
    int len = n;
    int start = 0;
    DSU dsu(lcp.size());

    for (const auto& lcpp : indexedLcp) {
        int place = lcpp.first;
        dsu.unionSets(place, place + 1);
        int size = dsu.getSize(place);
        long long newVal = static_cast<long long>(lcpp.second) * size;
        if (maxVal < newVal) {
            maxVal = newVal;
            len = lcpp.second;
            start = suf[lcpp.first];
        }
    }

    cout << maxVal << endl;
    cout << len << endl;

    for (int i = start; i < start + len; i++) {
        cout << a[i] + 1 << " ";
    }
    cout << endl;
}

int main() {
    solve();

    return 0;
}
