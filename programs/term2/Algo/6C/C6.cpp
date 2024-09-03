#include <iostream>
#include <vector>

using namespace std;

struct my_pair {
    int value;
    int weight;

    my_pair(int value, int weight) : value(value), weight(weight) {}
};

struct my_data {
    int time_in;
    int time_out;
    int weight;

    vector<int> data;

};

struct tree {

    const int e = 1000111;

    int timer;

    const int max_size = 200011;

    my_data *data;

    my_pair **table;

    tree() {
        timer = 0;
        data = (my_data *)malloc(sizeof(my_data) * max_size);
        table = (my_pair **)malloc(sizeof(my_pair *) * max_size);
        for (int i = 0; i < max_size; i++) {
            table[i] = (my_pair *)malloc(sizeof(my_pair) * 21);
        }
    };

    void dfs(int size) {
        dfs (0, 0, size);
    }

    void dfs(int index, int value, int table_size) {
        data[index].time_in = timer;
        timer++;
        table[index][0] = my_pair(value, data[index].weight);
        for (int i = 1; i < table_size; i++) {
            my_pair fir = table[index][i - 1];
            my_pair sec = table[fir.value][i - 1];
            table[index][i] = my_pair(sec.value, min(fir.weight, sec.weight));
        }

        for (int v: data[index].data) {
            if (v != value) {
                dfs(v, index, table_size);
            }
        }

        data[index].time_out = timer - 1;
    }

    bool compare(int a, int b) const {
        return data[a].time_in <= data[b].time_in && data[a].time_out >= data[b].time_out;
    }

    int get_min(int first, int second, int st_size) const {
        int min_weight = e;
        int tmp = first;

        if (!compare(first, second)) {
            for (int i = st_size - 1; i >= 0; i--) {
                if (!compare(table[first][i].value, second)) {
                    min_weight = min(min_weight, table[first][i].weight);
                    first = table[first][i].value;
                }
            }

            min_weight = min(min_weight, table[first][0].weight);
            tmp = table[first][0].value;
        }

        if (tmp != second) {
            for (int i = st_size - 1; i >= 0; i--) {
                if (!compare(table[second][i].value, tmp)) {
                    min_weight = min(min_weight, table[second][i].weight);
                    second = table[second][i].value;
                }
            }
            min_weight = min(min_weight, table[second][0].weight);
        }

        return min_weight;
    }

    void add(int index, int value, int weight) const {
        data[value].data.push_back(index);
        data[index].data.push_back(value);
        data[index].weight = weight;
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

    int log = log2(n);

    tree tree;

    int value, weight;

    for (int i = 1; i < n; ++i) {
        cin >> value >> weight;
        tree.add(i, value - 1, weight);
    }

    tree.dfs(log + 1);

    int m;
    cin >> m;

    int first;
    int second;

    for (int i = 0; i < m; i++) {
        cin >> first >> second;
        cout << tree.get_min(first - 1, second - 1, log + 1) << endl;
    }

    return 0;
}