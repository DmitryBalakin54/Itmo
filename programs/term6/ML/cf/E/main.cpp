
#include <iostream>
#include <vector>
#include <optional>
#include <cassert>
#include <cmath>
#include <cstddef>
#include <memory>
#include <thread>

using namespace std;


struct node {
    vector<node *> inputs;
    vector<vector<double>> value;
    vector<vector<double>> diff;

    virtual void compute() = 0;

    virtual void spread_diff() = 0;

    void initialize_diff() {
        diff.assign(value.size(), vector<double>(value[0].size(), 0));
    }

    void read_diff() {
        for (auto &row : diff) {
            for (auto &cell : row) {
                int x;
                cin >> x;
                cell = x;
            }
        }
    }

    explicit node(vector<node *> inputs) : inputs{move(inputs)}, value{} {}
};

struct var : node {
    var() : node(vector<node *>{}) {}

    void set_data(vector<vector<double>> data) {
        value = move(data);
    }

    void compute() final {
        initialize_diff();
    }

    void spread_diff() final {}
};

struct tnh : node {
    explicit tnh(node *source) : node(vector<node *>{source}) {}

    void compute() final {
        //assert(inputs.size() == 1);
//    inputs.front()->compute();
        value = inputs.front()->value;
        for (auto &row : value) {
            for (auto &cell : row) {
                cell = tanh(cell);
            }
        }
        initialize_diff();
    }

    void spread_diff() final {
        for (size_t i = 0; i < value.size(); ++i) {
            for (size_t j = 0; j < value[0].size(); ++j) {
                auto cur_value = value[i][j];
                inputs.front()->diff[i][j] += (1 - cur_value * cur_value) * diff[i][j];
            }
        }
    }
};

struct sigm : node {
    explicit sigm(node *source) : node(vector<node *>{source}) {}

    void compute() final {
        value = inputs.front()->value;
        for (auto &row : value) {
            for (auto &cell : row) {
                cell = 1.0 / (1 + exp(-cell));
            }
        }
        initialize_diff();
    }

    void spread_diff() final {
        for (size_t i = 0; i < value.size(); ++i) {
            for (size_t j = 0; j < value[0].size(); ++j) {
                auto cur_value = value[i][j];
                inputs.front()->diff[i][j] += cur_value * (1 - cur_value) * diff[i][j];
            }
        }
    }
};

struct rlu : node {
    double inv_alpha;

    rlu(double inv_alpha, node *source) : node(vector<node *>{source}), inv_alpha{inv_alpha} {}

    void compute() final {
        //assert(inputs.size() == 1);
//    inputs.front()->compute();
        value = inputs.front()->value;
        for (auto &row : value) {
            for (auto &cell : row) {
                if (cell < 0) {
                    cell /= inv_alpha;
                }
            }
        }
        initialize_diff();
    }

    void spread_diff() final {
        for (size_t i = 0; i < value.size(); ++i) {
            for (size_t j = 0; j < value[0].size(); ++j) {
                auto cur_input = inputs.front()->value[i][j];
                double multiplier;
                if (cur_input >= 0) {
                    multiplier = 1.0;
                } else {
                    multiplier = 1.0 / inv_alpha;
                }
                inputs.front()->diff[i][j] += multiplier * diff[i][j];
            }
        }
    }
};

struct mul : node {
    mul(node *a, node *b) : node(vector<node *>{a, b}) {}

    void compute() final {
        //assert(inputs.size() == 2);
//    inputs[0]->compute();
//    inputs[1]->compute();
        auto &a = inputs[0]->value;
        auto &b = inputs[1]->value;
        auto n = a.size();
        auto m = a[0].size();
        //assert(m == b.size());
        auto k = b[0].size();
        value = vector<vector<double>>(n, vector<double>(k, 0));
        for (size_t i = 0; i < n; ++i) {
            for (size_t j = 0; j < k; ++j) {
                for (size_t t = 0; t < m; ++t) {
                    value[i][j] += a[i][t] * b[t][j];
                }
            }
        }
        initialize_diff();
    }

    void spread_diff() final {
        auto &a = inputs[0]->value;
        auto &b = inputs[1]->value;
        auto &da = inputs[0]->diff;
        auto &db = inputs[1]->diff;
        auto n = a.size();
        auto m = a[0].size();
        auto k = b[0].size();
        for (size_t i = 0; i < n; ++i) {
            for (size_t j = 0; j < m; ++j) {
                double cur_diff = 0;
                for (size_t t = 0; t < k; ++t) {
                    cur_diff += diff[i][t] * b[j][t];
                }
                da[i][j] += cur_diff;
            }
        }
        for (size_t i = 0; i < m; ++i) {
            for (size_t j = 0; j < k; ++j) {
                double cur_diff = 0;
                for (size_t t = 0; t < n; ++t) {
                    cur_diff += a[t][i] * diff[t][j];
                }
                db[i][j] += cur_diff;
            }
        }
    }
};

struct sum : node {
    using node::node;

    void compute() final {
        //assert(!inputs.empty());
//    inputs.front()->compute();
        auto n = inputs.front()->value.size();
        auto m = inputs.front()->value[0].size();
        //assert(n > 0 && m > 0);
        value = vector<vector<double>>(n, vector<double>(m, 0));
        for (auto input : inputs) {
//      input->compute();
            auto &cur = input->value;
            //assert(cur.size() == n);
            //assert(cur[0].size() == m);
            for (size_t i = 0; i < n; ++i) {
                for (size_t j = 0; j < m; ++j) {
                    value[i][j] += cur[i][j];
                }
            }
        }
        initialize_diff();
    }

    void spread_diff() final {
        for (size_t i = 0; i < value.size(); ++i) {
            for (size_t j = 0; j < value[0].size(); ++j) {
                for (auto input : inputs) {
                    input->diff[i][j] += diff[i][j];
                }
            }
        }
    }
};

struct had : node {
    using node::node;

    void compute() final {
        //assert(!inputs.empty());
//    inputs.front()->compute();
        auto n = inputs.front()->value.size();
        auto m = inputs.front()->value[0].size();
        //assert(n > 0 && m > 0);
        value = vector<vector<double>>(n, vector<double>(m, 1));
        for (auto input : inputs) {
//      input->compute();
            auto &cur = input->value;
            //assert(cur.size() == n);
            //assert(cur[0].size() == m);
            for (size_t i = 0; i < n; ++i) {
                for (size_t j = 0; j < m; ++j) {
                    value[i][j] *= cur[i][j];
                }
            }
        }
        initialize_diff();
    }

    void spread_diff() final {
        for (size_t i = 0; i < value.size(); ++i) {
            for (size_t j = 0; j < value[0].size(); ++j) {
                for (size_t k = 0; k < inputs.size(); ++k) {
                    double multiplier = 1;
                    for (size_t t = 0; t < inputs.size(); ++t) {
                        if (t != k) {
                            multiplier *= inputs[t]->value[i][j];
                        }
                    }
                    inputs[k]->diff[i][j] += multiplier * diff[i][j];
                }
            }
        }
    }
};

struct network {
    vector<unique_ptr<node>> nodes;

    node *get_node(int pos) {
        return nodes[pos].get();
    }

    [[nodiscard]] vector<node *> get_inputs(vector<size_t> const &inputs_idxs) const {
        vector<node *> inputs;
        inputs.reserve(inputs_idxs.size());
        for (auto idx : inputs_idxs) {
            inputs.push_back(nodes[idx].get());
        }
        return inputs;
    }

    void add_node(node *new_node) {
        nodes.emplace_back(new_node);
    }

    void print_node(size_t idx) {
//    nodes[idx]->compute();
        for (auto &row : nodes[idx]->value) {
            for (auto cell : row) {
                cout << cell << " ";
            }
            cout << "\n";
        }
    }

    void print_diff(size_t idx) {
        for (auto &row : nodes[idx]->diff) {
            for (auto cell : row) {
                cout << cell << " ";
            }
            cout << "\n";
        }
    }

    void compute() {
        for (auto &ptr : nodes) {
            ptr->compute();
        }
    }

    void backprop() {
        for (auto it = nodes.rbegin(); it != nodes.rend(); ++it) {
            it->get()->spread_diff();
        }
    }
};

int main() {
    network net;
    int n;
    cin >> n;
    // first 4 * 3 nodes are 𝑊𝑓 , 𝑈𝑓, 𝐵𝑓, 𝑊𝑖, 𝑈𝑖, 𝐵𝑖, 𝑊𝑜, 𝑈𝑜, 𝐵𝑜, 𝑊𝑐, 𝑈𝑐, 𝐵𝑐.
    for (int i = 0; i < 4; ++i) {
        for (int t = 0; t < 2; ++t) {
            vector<vector<double>> matr(n, vector<double>(n));
            for (int j = 0; j < n; ++j) {
                for (int k = 0; k < n; ++k) {
                    int x;
                    cin >> x;
                    matr[j][k] = x;
                }
            }
            auto *matr_node = new var();
            matr_node->set_data(move(matr));
            net.add_node(matr_node);
        }
        vector<vector<double>> b(n, vector<double>(1));
        for (int j = 0; j < n; ++j) {
            int x;
            cin >> x;
            b[j][0] = x;
        }
        auto *b_node = new var();
        b_node->set_data(move(b));
        net.add_node(b_node);
    }
    int m;
    cin >> m;
    // ℎ0, 𝑐0
    for (int i = 0; i < 2; ++i) {
        vector<vector<double>> vec(n, vector<double>(1));
        for (int j = 0; j < n; ++j) {
            int x;
            cin >> x;
            vec[j][0] = x;
        }
        auto *vec_node = new var();
        vec_node->set_data(move(vec));
        net.add_node(vec_node);
    }
    constexpr int NODES_START = 4 * 3 + 2;
    constexpr int NODE_SIZE = 1 + 3 * 4 + 8;
    // 𝑐0, ℎ0
    swap(net.nodes[NODES_START - 1], net.nodes[NODES_START - 2]);
    vector<int> os = {-1000'000'000}; // to start real index from 1
    vector<int> cs = {NODES_START - 2};
    vector<int> hs = {NODES_START - 1};
    vector<int> xs = {-1000'000'000}; // to start real index from 1
    for (int i = 0; i < m; ++i) {
        vector<vector<double>> input(n, vector<double>(1));
        for (int j = 0; j < n; ++j) {
            int x;
            cin >> x;
            input[j][0] = x;
        }
        auto *input_node = new var();
        input_node->set_data(move(input));
        net.add_node(input_node);
        int start_pos = NODES_START + NODE_SIZE * i;
        int prev_h_pos = start_pos - 1;
        int prev_c_pos = start_pos - 2;
        int sums[4]{};
        for (int j = 0; j < 4; ++j) {
            int Wx_pos = net.nodes.size();
            net.add_node(new mul(net.get_node(j * 3), input_node));
            int Uh_pos = net.nodes.size();
            net.add_node(new mul(net.get_node(j * 3 + 1), net.get_node(prev_h_pos)));
            sums[j] = net.nodes.size();
            net.add_node(new sum({net.get_node(Wx_pos), net.get_node(Uh_pos), net.get_node(j * 3 + 2)}));
        }
        int f_pos = net.nodes.size();
        net.add_node(new sigm(net.get_node(sums[0])));
        int i_pos = net.nodes.size();
        net.add_node(new sigm(net.get_node(sums[1])));
        int o_pos = net.nodes.size();
        net.add_node(new sigm(net.get_node(sums[2])));
        int tahn_pos = net.nodes.size();
        net.add_node(new tnh(net.get_node(sums[3])));
        int ithan_pos = net.nodes.size();
        net.add_node(new had({net.get_node(i_pos), net.get_node(tahn_pos)}));
        int fprev_c_pos = net.nodes.size();
        net.add_node(new had({net.get_node(f_pos), net.get_node(prev_c_pos)}));
        int cur_c_pos = net.nodes.size();
        net.add_node(new sum({net.get_node(fprev_c_pos), net.get_node(ithan_pos)}));
        int cur_h_pos = net.nodes.size();
        net.add_node(new had({net.get_node(o_pos), net.get_node(cur_c_pos)}));
        os.push_back(o_pos);
        cs.push_back(cur_c_pos);
        hs.push_back(cur_h_pos);
        xs.push_back(start_pos);
    }

    cout << fixed;
    cout.precision(12);

    net.compute();
    net.get_node(hs[m])->read_diff();
    net.get_node(cs[m])->read_diff();
    for (int i = m; i >= 1; --i) {
        net.get_node(os[i])->read_diff();
    }
    net.backprop();

    for (int i = 1; i <= m; ++i) {
        net.print_node(os[i]);
    }
    net.print_node(hs[m]);
    net.print_node(cs[m]);
    for (int i = m; i >= 1; --i) {
        net.print_diff(xs[i]);
    }
    net.print_diff(hs[0]);
    net.print_diff(cs[0]);
    for (int i = 0; i < 4 * 3; ++i) {
        net.print_diff(i);
    }
    return 0;
}