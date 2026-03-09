#include <bits/stdc++.h>

using namespace std;

using Vec = vector<int>;
using Mat = vector<Vec>;

struct Vertex;

struct Edge {
    Vertex* to;
    int bit;
};

struct Vertex {
    Vertex* parent = nullptr;
    double best = -1.0;
    vector<Edge> out;
    vector<pair<int,int>> state;
};

int N, K;
Mat G;
vector<vector<Vertex*>> trellis;

inline double bpsk(int b) {
    if (b == 0) {
        return 1.0;
    }

    return -1.0;
}

double sampleNormal(double mu, double sigma) {
    static default_random_engine eng(random_device{}());

    return normal_distribution<double>(mu, sigma)(eng);
}

Vec encode(const Vec& u) {
    Vec c(N, 0);

    for (int j = 0; j < N; j++) {
        for (int i = 0; i < K; i++) {
            c[j] ^= (G[i][j] & u[i]);
        }
    }

    return c;
}

Mat toMSF() {
    Mat M = G;
    int r = 0;

    for (int c = 0; c < N && r < K; c++) {
        int p = -1;

        for (int i = r; i < K; i++) {
            if (M[i][c]) {
                p = i;
                break;
            }
        }

        if (p < 0) {
            continue;
        }

        swap(M[p], M[r]);

        for (int i = 0; i < K; i++) {
            if (i != r && M[i][c]) {
                for (int j = 0; j < N; j++) {
                    M[i][j] ^= M[r][j];
                }
            }
        }

        r++;
    }

    vector<int> free(K, 1);
    int col = N - 1;

    for (int used = 0; used < K; used++, col--) {
        int cnt = 0;

        for (int i = 0; i < K; i++) {
            if (free[i] && M[i][col]) {
                cnt++;
            }
        }

        if (cnt == 0) {
            used--;
            continue;
        }

        int idx = K - 1;

        while (!(free[idx] && M[idx][col])) {
            idx--;
        }

        free[idx] = 0;

        for (int i = 0; i < idx; i++) {
            if (free[i] && M[i][col]) {
                for (int j = 0; j < N; j++) {
                    M[i][j] ^= M[idx][j];
                }
            }
        }
    }

    return M;
}

vector<pair<int,int>> computeRanges(const Mat& M) {
    vector<pair<int,int>> spans(K);

    for (int i = 0; i < K; i++) {
        int l = 0;
        int r = N - 1;

        while (!M[i][l]) {
            l++;
        }

        while (!M[i][r]) {
            r--;
        }

        if (l < r) {
            r--;
        }

        spans[i] = {l, r};
    }

    return spans;
}

bool parity(const vector<int>& col, const vector<pair<int,int>>& st) {
    bool v = false;

    for (auto& p : st) {
        v ^= (p.first && col[p.second]);
    }

    return v;
}

void buildGraph() {
    Mat M = toMSF();
    auto spans = computeRanges(M);

    trellis.assign(N + 1, {});
    trellis[0].push_back(new Vertex());

    for (int pos = 0; pos < N; pos++) {
        map<vector<pair<int,int>>, Vertex*> next;
        vector<int> col(K);

        for (int i = 0; i < K; i++) {
            col[i] = M[i][pos];
        }

        int start = -1;

        for (int i = 0; i < K; i++) {
            if (spans[i].first == pos) {
                start = i;
                break;
            }
        }

        for (Vertex* v : trellis[pos]) {
            vector<vector<pair<int,int>>> candidates;
            vector<int> bits;

            if (start < 0) {
                candidates.push_back(v->state);
                bits.push_back(parity(col, v->state));
            } else {
                auto s0 = v->state;
                auto s1 = v->state;

                s0.emplace_back(0, start);
                s1.emplace_back(1, start);

                int p = parity(col, s1);

                candidates.push_back(s0);
                bits.push_back(!p);

                candidates.push_back(s1);
                bits.push_back(p);
            }

            for (size_t i = 0; i < candidates.size(); i++) {
                auto st = candidates[i];
                int bit = bits[i];

                for (size_t j = 0; j < st.size(); j++) {
                    if (pos > spans[st[j].second].second) {
                        st.erase(st.begin() + j);
                        break;
                    }
                }

                auto it = next.find(st);

                if (it == next.end()) {
                    Vertex* nv = new Vertex();
                    nv->state = st;
                    it = next.insert({st, nv}).first;
                }

                v->out.push_back({it->second, bit});
            }
        }

        for (auto& e : next) {
            trellis[pos + 1].push_back(e.second);
        }
    }
}

Vec decode(const vector<double>& rx) {
    const double INF = -1.0;
    trellis[0][0]->best = 0.0;

    for (size_t i = 1; i < trellis.size(); i++) {
        for (Vertex* v : trellis[i]) {
            v->parent = nullptr;
            v->best = INF;
        }
    }

    for (size_t i = 0; i < rx.size(); i++) {
        for (Vertex* v : trellis[i]) {
            for (auto& e : v->out) {
                double diff = rx[i] - bpsk(e.bit);
                double cand = v->best + diff * diff;

                if (e.to->best == INF || e.to->best > cand) {
                    e.to->best = cand;
                    e.to->parent = v;
                }
            }
        }
    }

    Vec res;
    Vertex* cur = trellis.back()[0];

    for (int layer = trellis.size() - 1; layer > 0 && cur; layer--) {
        Vertex* p = cur->parent;
        int b = 0;

        if (p) {
            for (auto& e : p->out) {
                if (e.to == cur) {
                    b = e.bit;
                    break;
                }
            }
        }

        res.push_back(b);
        cur = p;
    }

    reverse(res.begin(), res.end());

    return res;
}

void randomMessage(Vec& u) {
    static mt19937 gen(random_device{}());

    for (int& x : u) {
        x = gen() & 1;
    }
}

double simulate(double snrDb, int limit, int maxErr) {
    double lin = pow(10.0, -snrDb / 10.0);
    double sigma = sqrt(0.5 * N / K * lin);

    int err = 0;
    int tot = 0;

    while (tot < limit && err < maxErr) {
        Vec u(K);
        randomMessage(u);

        Vec c = encode(u);
        vector<double> y(N);

        for (int i = 0; i < N; i++) {
            y[i] = sampleNormal(0.0, sigma) + bpsk(c[i]);
        }

        if (decode(y) != c) {
            err++;
        }

        tot++;
    }

    return double(err) / tot;
}

int main() {
    ifstream in("input.txt");
    ofstream out("output.txt");

    in >> N >> K;
    G.assign(K, Vec(N));

    for (int i = 0; i < K; i++) {
        for (int j = 0; j < N; j++) {
            in >> G[i][j];
        }
    }

    buildGraph();

    for (auto& lvl : trellis) {
        out << lvl.size() << " ";
    }

    string cmd;
    while (in >> cmd) {
        out << "\n";

        if (cmd == "Encode") {
            Vec u(K);

            for (int i = 0; i < K; i++) {
                in >> u[i];
            }

            Vec c = encode(u);

            for (int b : c) {
                out << b << " ";
            }

        } else if (cmd == "Decode") {
            vector<double> y(N);
            for (int i = 0; i < N; i++) {
                in >> y[i];
            }

            Vec c = decode(y);

            for (int b : c) {
                out << b << " ";
            }
        } else if (cmd == "Simulate") {
            double snr;
            int it;
            int er;
            in >> snr >> it >> er;

            out << simulate(snr, it, er);
        }
    }

    return 0;
}
