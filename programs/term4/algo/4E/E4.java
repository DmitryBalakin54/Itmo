import java.io.*;
import java.util.*;

public class E4 {

    static class Pair<T, V> {
        T a;
        V b;

        Pair(T a, V b) {
            this.a = a;
            this.b = b;
        }

        T getKey() {
            return a;
        }

        V getValue() {
            return b;
        }
    }

    static class Edge {
        int b, e;
        int c, f, p;
        int n;
        Edge backEdge;

        Edge(int b, int e, int c, int p, int n) {
            this.b = b;
            this.e = e;
            this.c = c;
            this.f = 0;
            this.p = p;
            this.n = n;
            this.backEdge = null;
        }
    }

    static class MinCostMaxFlowSolver {
        private List<List<Edge>> edges;
        private int[] d;
        private Edge[] from;
        private int INF;
        private int n;

        MinCostMaxFlowSolver(int n, int INF) {
            this.INF = INF;
            this.n = n;
            d = new int[n];
            from = new Edge[n];
            edges = new ArrayList<>(n);
            for (int i = 0; i < n; i++) {
                edges.add(new ArrayList<>());
            }
        }

        void addEdge(int b, int e, int c, int p, int n) {
            Edge e1 = new Edge(b, e, c, p, n);
            Edge e2 = new Edge(e, b, 0, -p, n);
            e1.backEdge = e2;
            e2.backEdge = e1;
            edges.get(b).add(e1);
            edges.get(e).add(e2);
        }

        boolean bfs(int s, int t) {
            Arrays.fill(d, INF);
            d[s] = 0;
            boolean updated = true;

            for (int i = 0; i < n - 1 && updated; i++) {
                updated = false;
                for (int u = 0; u < n; u++) {
                    if (d[u] == INF) continue;
                    for (Edge e : edges.get(u)) {
                        if (e.c - e.f > 0 && d[e.e] > d[u] + e.p) {
                            d[e.e] = d[u] + e.p;
                            from[e.e] = e;
                            updated = true;
                        }
                    }
                }
            }

            return d[t] != INF;
        }

        Pair<Integer, Long> push(int s, int v, int cMin) {
            if (v == s) {
                return new Pair<>(cMin, 0L);
            }

            Edge e = from[v];
            Pair<Integer, Long> add = push(s, e.b, Math.min(cMin, e.c - e.f));
            if (add.getKey() > 0) {
                e.f += add.getKey();
                e.backEdge.f -= add.getKey();
                add = new Pair<>(add.getKey(), add.getValue() + add.getKey() * e.p);
            }
            return add;
        }

        long run(int s, int t) {
            long price = 0;
            while (bfs(s, t)) {
                Pair<Integer, Long> add = push(s, t, INF);
                price += add.getValue();
            }
            return price;
        }

        void ans(List<Integer> ans, int n) {
            for (int i = 2; i < n; ++i) {
                for (Edge e : edges.get(i)) {
                    if (e.n != -1 && e.f == 1) {
                        ans.set(e.n, 1);
                    }
                }
            }
        }
    }

    static class Task {
        int b, e, p, n;

        Task(int b, int e, int p, int n) {
            this.b = b;
            this.e = e;
            this.p = p;
            this.n = n;
        }
    }

    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        // BufferedReader reader = new BufferedReader(new FileReader("in.txt"));
        // PrintWriter writer = new PrintWriter(new FileWriter("out.txt"));

        int n, k;
        String[] input = reader.readLine().split(" ");
        n = Integer.parseInt(input[0]);
        k = Integer.parseInt(input[1]);

        List<Task> tasks = new ArrayList<>();
        MinCostMaxFlowSolver minCost = new MinCostMaxFlowSolver(2 * n + 3, 1000000001);

        for (int i = 0; i < n; ++i) {
            input = reader.readLine().split(" ");
            int b = Integer.parseInt(input[0]);
            int e = Integer.parseInt(input[1]);
            int p = Integer.parseInt(input[2]);
            tasks.add(new Task(b, b + e, p, i));
        }

        tasks.sort(Comparator.comparingInt(a -> a.b));

        for (int i = 2; i <= n + 1; ++i) {
            Task task = tasks.get(i - 2);
            minCost.addEdge(i, n + i, 1, -task.p, task.n);
            minCost.addEdge(1, i, k, 0, -1);
            minCost.addEdge(n + i, 2 * n + 2, 1, 0, -1);
        }

        minCost.addEdge(0, 1, k, 0, -1);

        for (int i = 2; i < n + 1; ++i) {
            minCost.addEdge(i, i + 1, k, 0, -1);
            for (int j = i + 1; j <= n + 1; ++j) {
                if (tasks.get(i - 2).e <= tasks.get(j - 2).b) {
                    minCost.addEdge(n + i, j, 1, 0, -1);
                    break;
                }
            }
        }

        minCost.run(0, 2 * n + 2);
        List<Integer> ans = new ArrayList<>(Collections.nCopies(n, 0));
        minCost.ans(ans, n + 2);
        for (int e : ans) {
            System.out.print(e + " ");
        }
        System.out.println();
    }
}
