#include <iostream>
#include <vector>
#include <algorithm>

class Node {
public:
    std::vector<Node*> cld;
    int cnt;

    Node() : cld(26, nullptr), cnt(0) {}
};

class Dct {
public:
    Node* root;
    std::vector<std::string> sorted_wrds;

    Dct() : root(new Node()) {}

    void add(const std::string& word) {
        Node* node = root;
        for (char ch : word) {
            if (!node->cld[ch - 'a']) {
                node->cld[ch - 'a'] = new Node();
            }
            node = node->cld[ch - 'a'];
            node->cnt++;
        }
        sorted_wrds.push_back(word);
    }

    int cnt(const std::string& word) {
        int count = 0;
        for (const std::string& w : sorted_wrds) {
            if (w < word) {
                count++;
            }
        }
        return count;
    }
};

int main() {
    std::ios::sync_with_stdio(false);
    std::cin.tie(0);
    std::cout.tie(0);

    int n, q;
    std::cin >> n >> q;

    std::string init;
    std::cin >> init;

    Dct lxg_dct;

    for (int i = 0; i < n; ++i) {
        std::string word;
        std::cin >> word;
        lxg_dct.add(word);
    }

    std::vector<std::pair<int, char>> mods(q);
    for (int i = 0; i < q; ++i) {
        std::cin >> mods[i].first >> mods[i].second;
    }

    int cnt0 = lxg_dct.cnt(init);
    std::cout << cnt0 << std::endl;

    for (const auto& mod : mods) {
        int ki = mod.first - 1;
        char ci = mod.second;

        init = init.substr(0, ki) + std::string(init.length() - ki, ci);

        int countAfterMod = lxg_dct.cnt(init);
        std::cout << countAfterMod << std::endl;
    }

    return 0;
}
