#include <iostream>

using namespace std;

typedef long long int llu;

struct dec_node {
    llu x;
    llu y;
    llu sum;

    dec_node *left_son, *right_son;

    explicit dec_node(llu x) {
        this->x = x;
        y = rand();
        sum = x;
        left_son = nullptr;
        right_son = nullptr;
    }
};

struct dec_tree {

    dec_node *root;

    dec_tree() {
        root = nullptr;
    }

    static llu get_sum(dec_node *node) {
        if (!node) {
            return 0;
        }
        return node->sum;
    }

    bool exists(dec_node *node, llu x) {
        if (!node) {
            return false;
        }

        if (node->x == x){
            return true;
        }

        if (node->x <= x) {
            return exists(node->right_son, x);
        } else {
            return exists(node->left_son, x);
        }
    }

    static void calculate(dec_node **node) {
        if (!(*node)) {
            return;
        }
        (*node)->sum = (*node)->x + get_sum((*node)->left_son) + get_sum((*node)->right_son);
    }

    void split(dec_node *node, dec_node **left, dec_node **right, llu x) {
        if (!node) {
            (*left) = nullptr;
            (*right) = nullptr;
            return;
        }

        dec_node *a = nullptr;
        dec_node *b = nullptr;
        if (node->x <= x) {
            split(node->right_son, &a, &b, x);
            node->right_son = a;
            (*right) = b;
            (*left) = node;
        } else {
            split(node->left_son, &a, &b, x);
            node->left_son = b;
            (*left) = a;
            (*right) = node;
        }

        calculate(&node);
    }

    void merge(dec_node **node, dec_node *left, dec_node *right) {
        if (!right) {
            (*node) = left;
            return;
        }

        if (!left) {
            (*node) = right;
            return;
        }

        if (left->y <= right->y) {
            merge(&right->left_son, left, right->left_son);
            (*node) = right;
        } else {
            merge(&left->right_son, left->right_son, right);
            (*node) = left;
        }
        calculate(node);
    }

    void insert(dec_node **node, llu x) {
        if (exists(*node, x)) {
            return;
        }

        auto *n = new dec_node(x);
        dec_node *a = nullptr;
        dec_node *b = nullptr;

        split(*node, &a, &b, x);

        merge(&a, a, n);
        merge(node, a, b);
    }

    llu get_sum(dec_node *node, llu left, llu right) {
        dec_node *a = nullptr;
        dec_node *b = nullptr;
        dec_node *c = nullptr;

        split(node, &a, &b, left - 1);
        split(b, &b, &c, right);

        llu res = (!b ? 0 : b->sum);

        merge(&b, b, c);
        merge(&node, a, b);
        return res;
    }

    void add (long long x) {
        insert(&root, x);
    }

    llu sum(llu  left, llu  right) {
        return get_sum(root, left, right);
    }
};

int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);
    cout.tie(0);

    dec_tree tree = dec_tree();

    llu  n;
    cin >> n;

    bool last_is_sum = false;
    llu last_res = 0;
    char ch;
    llu x;
    llu left, right;

    for (llu i = 0; i < n; i++) {

        cin >> ch;
        if (ch == '+') {
            cin >> x;

            if (last_is_sum) {
                tree.add((last_res + x) % 1000000000);
            } else {
                tree.add(x);
            }

            last_is_sum = false;
        } else if (ch == '?') {
            cin >> left >> right;

            last_res = tree.sum(left, right);

            cout << last_res << endl;
            last_is_sum = true;
        }
    }
    return 0;
}