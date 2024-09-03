#include <iostream>
#include <vector>
#include <ios>

class TrieNode {
public:
    TrieNode* clds[2];
    int cnt;

    TrieNode() : cnt(0) {
        clds[0] = clds[1] = nullptr;
    }
};

void add(TrieNode* root, int num) {
    TrieNode* node = root;
    for (int i = 31; i >= 0; --i) {
        int bit = (num >> i) & 1;
        if (!node->clds[bit]) {
            node->clds[bit] = new TrieNode();
        }
        node = node->clds[bit];
        node->cnt += 1;
    }
}

void pop(TrieNode* root, int num) {
    TrieNode* node = root;
    for (int i = 31; i >= 0; --i) {
        int bit = (num >> i) & 1;
        node = node->clds[bit];
        node->cnt -= 1;
    }
}

int max_xor(TrieNode* root, int num) {
    TrieNode* nd = root;
    int res = 0;
    for (int i = 31; i >= 0; --i) {
        int bit = (num >> i) & 1;
        int neg_bit = 1 - bit;
        if (nd->clds[neg_bit] && nd->clds[neg_bit]->cnt > 0) {
            res |= (1 << i);
            nd = nd->clds[neg_bit];
        } else {
            nd = nd->clds[bit];
        }
    }
    return res;
}

void ans(const int n) {
    TrieNode root;
    add(&root, 0);

    for (int i = 0; i < n; i++) {
        char op;
        std::string val_s;
        std::cin >> op >> val_s;
        int num = std::stoi(val_s);
        if (op == '+') {
            add(&root, num);
        } else if (op == '-') {
            pop(&root, num);
        } else if (op == '?') {
            std::cout << max_xor(&root, num) << "\n";
        }
    }
}

int main() {
    std::ios::sync_with_stdio(false);
    std::cin.tie(0);
    std::cout.tie(0);
    int q;
    std::cin >> q;
    ans(q);
    return 0;
}
