#include<iostream>
#include<malloc.h>

using namespace std;
struct min_sparse_table {

    int *log2;
    int *table;
    int n;
    int m;

    int index(int i, int j) {
        return j * n + i;
    }

    min_sparse_table(int n, int *array) {
        log2 = (int *)(malloc(sizeof(int) * (n + 1)));

        log2[1] = 0;
        for (int i = 2; i <= n; i++) {
            log2[i] = log2[i / 2] + 1;
        }

        this->n = n;
        m = log2[n] + 1;

        table = (int *)realloc(array, sizeof(int) * n * m);
        for (int i = n - 1; i >= 0; i--) {
            for (int j = 1; j < log2[n] + 1; j++) {
                table[index(i, j)] = min(table[index(i, j - 1)], table[index(min(i + (1 << (j - 1)), n - 1), j - 1)]);
            }
        }
    }

    int get_min(int left, int right) {
        if (left > right) {
            int tmp = left;
            left = right;
            right = tmp;
        }

        int log = log2[right - left + 1];

        return min(table[index(left, log)], table[index(right - (1 << log) + 1, log)]);
    }

};

int main() {

    int n;
    int m;
    int a1;

    cin >> n >> m >> a1;

    int u;
    int v;

    cin >> u >> v;

    int *array = (int *)malloc(sizeof(int) * n);
    array[0] = a1;
    for (int i = 1; i < n; i++) {
        array[i] = (23 * array[i - 1] + 21563) % 16714589;
    }

    min_sparse_table table = min_sparse_table(n, array);

    int res = table.get_min(u - 1, v - 1);
    for (int i = 1; i < m; i++) {
        u = ((17 * u + 751 + res + 2 * i) % n) + 1;
        v = ((13 * v + 593 + res + 5 * i) % n) + 1;
        res = table.get_min(u - 1, v - 1);
    }

    cout << u << " " << v << " " << res << endl;

    return 0;
}

