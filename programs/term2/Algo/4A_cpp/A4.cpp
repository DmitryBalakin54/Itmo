#include <vector>
#include <string>
#include <iostream>
#include <random>

using namespace std;
struct stupid_set {
    vector<vector<int>> data;
    int n = 8388607;
    int a = 2371 % n;
    int b = 17 % n;
    stupid_set()  {
        data = vector<vector<int>>(n);
        for (auto & i : data) {
            i = vector<int>();
        }
    }

    int hash(int x) {
        return abs((a + (((x % n) * b) % n) + (((x + a - b) / (a * b)) % n)) % n);
    }

    void add(int x) {
        if (exists(x)) {
            return;
        }
        data[hash(x)].push_back(x);
    }

    void delete_el(int x) {
        for (int i = 0; i < data[hash(x)].size(); i++) {
            if (data[hash(x)][i] == x) {
                data[hash(x)].erase(data[hash(x)].begin() + i);
                break;
            }
        }
    }

    bool exists(int x) {
        for (int i = 0; i < data[hash(x)].size(); i++) {
            if (data[hash(x)][i] == x) {
                return true;
            }
        }
        return false;
    }
};

struct stupid_set2 {
    uint8_t *pos_data;
    uint8_t *neg_data;
    int n = 8;

    stupid_set2()  {
        pos_data = (uint8_t *)malloc(sizeof(uint8_t) * 125000000);
        neg_data = (uint8_t *)malloc(sizeof(uint8_t) * 125000000);
    }


    void add(int x) {
        int val = x;
        if (val < 0) {
            val = -val;
        }

        int ind = val / n;
        int bit = val % n;
        uint8_t res = 1 << bit;

        if (x >= 0) {
            pos_data[ind] = res | pos_data[ind];
        } else {
            neg_data[ind] = res | neg_data[ind];
        }
    }

    void delete_el(int x) {
        int val = x;
        if (val < 0) {
            val = -val;
        }

        int ind = val / n;
        int bit = val % n;
        uint8_t res = 255 - (1 << bit);

        if (x >= 0) {
            pos_data[ind] = res & pos_data[ind];
        } else {
            neg_data[ind] = res & neg_data[ind];
        }
    }

    bool exists(int x) {
        int val = x;
        if (val < 0) {
            val = -val;
        }

        int ind = val / n;
        int bit = val % n;
        uint8_t res = 1 << bit;

        if (x >= 0) {
            res = res & pos_data[ind];
        } else {
            res = res & neg_data[ind];
        }
        return res > 0;
    }
};


struct stupid_set3 {
    uint8_t *data;
    int n = 8;

    stupid_set3()  {
        data = (uint8_t *)malloc(sizeof(uint8_t) * 125000000 * 2);
    }


    void add(int x) {
        x += 1000000000;
        int ind = x / n;
        int bit = x % n;
        //int bit = ((unsigned int )x << 29) >> 29;
        uint8_t res = 1 << bit;

        data[ind] = res | data[ind];
    }

    void delete_el(int x) {
        x += 1000000000;

        int ind = x / n;
        int bit = x % n;
        //int bit = ((unsigned int)x << 29) >> 29;
        uint8_t res = 255 - (1 << bit);

        data[ind] = res & data[ind];
    }

    bool exists(int x) {
        x += 1000000000;

        int ind = x / n;
        int bit = x % n;
        //int bit = ((unsigned int) x << 29) >> 29;
        uint8_t res = 1 << bit;

        res = res & data[ind];

        return res > 0;
    }
};




int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);
    cout.tie(0);
    string s;
    int x;
    stupid_set3 set;
    while (cin >> s) {
        cin >> x;
        if (s[0] == 'i') {
            set.add(x);
        } else if (s[0] == 'd') {
            set.delete_el(x);
        } else if (s[0] == 'e') {
            if (set.exists(x)) {
                cout << "true\n";
            } else {
                cout << "false\n";
            }
        }
    }
    return 0;
}