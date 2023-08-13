#include <iostream>
#include <vector>

typedef long long int lli;

using namespace std;

const lli m_mod = 1e9 + 239;

struct node {

    string key;
    string value;

    node *next{}, *prev{};

    node() = default;

    node(const string &key, const string &value) {
        this->key = key;
        this->value = value;
        next = nullptr;
        prev = nullptr;
    }


};


struct element {
    element(const string &key, const string &value) {
        this->key = key;
        this->value = value;
    }

    string key;
    string value;

    node *node{};
};

struct linked_hash_map {

    node *head;
    int nodes_size;
    vector<vector<element>> map;

    linked_hash_map() {
        map = vector<vector<element>>( 1e6);
        head = new node("abcdeabcdeDFcdeabcGF", "none");
        nodes_size = 0;
    }

    static lli mod(lli x) {
        return (x % m_mod + m_mod) % m_mod;
    }

    int get_hash(const string &s) const {
        lli hash = 0;
        lli p = 23909;
        lli p_pow = 1;

        for (char i: s) {
            hash += (i + 1) * p_pow;
            hash = mod(hash);

            p_pow *= p;
            p_pow = mod(p_pow);
        }

        return (int) hash % (int) map.size();
    }

    bool exists(const string &x) {
        int hash = get_hash(x);

        for (auto & j : map[hash]) {
            if (j.key == x) {
                return true;
            }
        }
        return false;
    }

    string get(const string &x) {
        int hash = get_hash(x);

        for (auto & j : map[hash]) {
            if (j.key == x) {
                return j.value;
            }
        }
        return "none";
    }

    void put(const string &key, const string &value) {
        int hash = get_hash(key);

        if (get(key) == value && exists(key)) {
            return;
        }

        for (auto & j : map[hash]) {
            if (j.key == key) {
                j.value = value;
                j.node->value = value;
                return;
            }
        }

        node *node_ = new node(key, value);

        nodes_size++;
        head->next = node_;
        node_->prev = head;

        element element_ = element(key, value);
        element_.node = node_;
        head = node_;
        map[hash].push_back(element_);
    }


    void re_head(int hash, int ind) {
        node *node = map[hash][ind].node;

        if (head == node) {
            head = node->prev;
        }

        if (node->prev != nullptr) {
            node->prev->next = node->next;
        }

        if (node->next != nullptr) {
            node->next->prev = node->prev;
        }
    }

    void delete_el(const string &x) {
        if (nodes_size == 0) {
            return;
        }

        if (!exists(x)) {
            return;
        }

        int hash = get_hash(x);

        for (int j = 0; j < (int) map[hash].size(); j++) {
            if (map[hash][j].key == x) {

                re_head(hash, j);

                //swap(map[hash][j], map[hash][map[hash].size() - 1]);

                element tmp = map[hash][j];
                map[hash][j] = map[hash][map[hash].size() - 1];
                map[hash][map[hash].size() - 1] = tmp;

                nodes_size--;
                map[hash].pop_back();

            }
        }
    }

    string next(const string &x) {
        int hash = get_hash(x);
        for (auto & j : map[hash]) {
            if (j.key == x) {
                if (j.node->next == nullptr) {
                    break;
                }
                return j.node->next->value;
            }
        }
        return "none";
    }

    string prev(const string &x) {

        int hash = get_hash(x);

        for (auto & j : map[hash]) {
            if (j.key == x) {
                if (j.node->prev == nullptr) {
                    break;
                }
                return j.node->prev->value;
            }
        }

        return "none";
    }

};


int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);
    cout.tie(0);
    string s;
    linked_hash_map map;
    while (cin >> s) {
        string x, y;
        if (s == "put") {
            cin >> x >> y;
            map.put(x, y);
        } else if (s == "delete") {
            cin >> x;
            map.delete_el(x);
        } else if (s == "get") {
            cin >> x;
            cout << map.get(x) << endl;
        } else if (s == "next") {
            cin >> x;
            cout << map.next(x) << endl;
        } else if (s == "prev") {
            cin >> x;
            cout << map.prev(x) << endl;
        }
    }
    return 0;
}