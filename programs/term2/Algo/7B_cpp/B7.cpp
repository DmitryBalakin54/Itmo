#include <bits/stdc++.h>

using namespace std;

struct my_data {
    int time_in;
    int time_out;
    int parent;
    vector<int> children;
    vector<int>jump;
};

struct my_tree {

    int timer;

    vector<my_data> data;
    vector<long long> t;

    static int pow_two(int value) {
        value--;
        for (int i = 1; i <= 16; i *= 2) {
            value |= value >> i;
        }
        return value + 1;
    }

    my_tree(int n, int log_2){
        timer = 0;
        data = vector<my_data>(n);
        for (auto & i : data) {
            i.children = vector<int>();
            i.jump = vector<int>(log_2 + 1);
        }
    }

    void add(int u, int v) {
        data[u - 1].children.push_back(v - 1);
        data[v - 1].children.push_back(u - 1);
    }

    void make_t() {
        int doSize = pow_two(data[0].time_out - data[0].time_in + 1);
        t = vector<long long>((unsigned long long) (2 * doSize - 1));

        for (int i = 0; i < doSize; i++) {
            t[doSize - 1 + i] = 0;
        }
        for (int i = doSize - 2; i >= 0; i--) {
            t[i] = t[get_left(i)] + t[get_right(i)];
        }

    }

    static int get_left(int i) {
        return 2 * i + 1;
    }

    static int get_right(int i) {
        return get_left(i) + 1;
    }

    static int get_parent(int i) {
        return (i - 1) / 2;
    }

    long long query(int index, int a, int b, int left, int right) {
        if (left >= b || right <= a) {
            return 0;
        }
        if (left >= a && right <= b) {
            return t[index];
        }
        int m = (left + right) / 2;
        return query(get_left(index), a, b, left, m) + query(get_right(index), a, b, m, right);
    }

    long long query(int index) {
        int b = data[index].time_out + 1;
        index = data[index].time_in;
        return query(0, index, b, 0, (int) ((t.size() + 1) / 2));
    }

    void update(int index) {
        t[index] = t[get_left(index)] + t[get_right(index)];
        if (index == 0) {
            return;
        }
        update(get_parent(index));
    }

    void add_v(int i, long long v) {
        i = data[i].time_in;
        int n = (int)((t.size() + 1) / 2);
        t[n - 1 + i] += v;
        if (n - 1 + i != 0) {
            update(get_parent(n - 1 + i));
        }
    }

    bool compare(int u, int v) {
        return data[u].time_in <= data[v].time_in && data[u].time_out >= data[v].time_out;
    }


    int lca(int a, int b) {
        if (compare(a, b)) {
            return a;
        }
        if (compare(b, a)) {
            return b;
        }
        for (size_t i = data[0].jump.size(); i-- > 0;) {
            if (!compare(data[a].jump[i], b)) {
                a = data[a].jump[i];
            }
        }
        return data[a].jump[0];
    }

    void add_op(int v, int u, int d) {
        int l = lca(v - 1, u - 1);
        add_v(v - 1, d);
        add_v(u - 1, d);
        add_v(l, -d);
        if (l != 0) {
            add_v(data[l].parent, -d);
        }
    }

    void dfs() {
        dfs(0, 0);
        make_t();
    }

    void dfs(int u, int parent) {
        data[u].time_in= timer;
        timer++;
        data[u].jump[0] = parent;
        data[u].parent = parent;

        for (size_t i = 1; i < data[0].jump.size(); i++) {
            data[u].jump[i] = data[data[u].jump[i - 1]].jump[i - 1];
        }

        for (int v : data[u].children) {
            if (v != parent) {
                dfs(v,  u);
            }
        }
        data[u].time_out = timer - 1;
    }


};

int log2(int n) {
    int res = 1;
    while ((1 << res) <= n) {
        res++;
    }
    return res;
}

int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);
    cout.tie(0);

    int n;
    cin >> n;

    my_tree tree = my_tree(n, log2(n));

    int u, v;
    for (int i = 0; i < n - 1; ++i) {
        cin >> u >> v;
        tree.add(u, v);
    }

    tree.dfs();

    int m, d;
    char op;
    cin >> m;
    for (int i = 0; i < m; i++) {
        cin >> op;
        if (op == '+') {
            cin >> v >> u >> d;
            tree.add_op(v, u, d);
        } else if (op == '?') {
            cin >> v;
            cout << tree.query(v - 1) << endl;
        }
    }

    return 0;
}