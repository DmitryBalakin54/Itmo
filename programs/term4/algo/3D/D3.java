import java.util.*;

public class D3 {
    static final int INF = Integer.MAX_VALUE;
    static int n, m, s, t;

    static class Edge {
        int u, v, f, c;

        Edge(int u, int v, int f, int c) {
            this.u = u;
            this.v = v;
            this.f = f;
            this.c = c;
        }
    }
    static List<Edge> edges = new ArrayList<>();
    static List<List<Integer>> graph = new ArrayList<>();
    static int[] d, p;

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        n = scanner.nextInt();
        m = scanner.nextInt();
        s = scanner.nextInt() - 1;
        t = scanner.nextInt() - 1;

        for (int i = 0; i < n; i++) {
            graph.add(new ArrayList<>());
        }

        d = new int[n];
        p = new int[n];

        for (int i = 0; i < m; i++) {
            int u = scanner.nextInt() - 1;
            int v = scanner.nextInt() - 1;
            addEdges(u, v);
        }

        double res = maxFlow();

        if (res < 2) {
            System.out.println("NO");
        } else {
            System.out.println("YES");
            List<Integer> fpath = new ArrayList<>();
            List<Integer> spath = new ArrayList<>();
            createPath(s, fpath);
            createPath(s, spath);
            for (int e : spath) {
                System.out.print((e + 1) + " ");
            }
            System.out.println();
            for (int e : fpath) {
                System.out.print((e + 1) + " ");
            }
        }

        scanner.close();
    }

    static void addEdges(int u, int v) {
        graph.get(u).add(edges.size());
        edges.add(new Edge(u, v, 0, 1));
        graph.get(v).add(edges.size());
        edges.add(new Edge(v, u, 0, 0));
    }

    static boolean bfs() {
        Arrays.fill(d, -1);
        d[s] = 0;

        Queue<Integer> q = new LinkedList<>();
        q.add(s);

        while (!q.isEmpty()) {
            int u = q.poll();

            for (int e : graph.get(u)) {
                Edge edge = edges.get(e);
                int v = edge.v;
                int f = edge.f;
                int c = edge.c;
                if (f < c && d[v] == -1) {
                    d[v] = d[u] + 1;
                    q.add(v);
                }
            }
        }
        return d[t] != -1;
    }

    static int dfs(int u, int minC) {
        if (u == t || minC == 0) {
            return minC;
        }
        for (; p[u] < graph.get(u).size(); p[u]++) {
            int e = graph.get(u).get(p[u]);
            Edge edge = edges.get(e);
            int v = edge.v;
            int f = edge.f;
            int c = edge.c;

            if (d[v] != d[u] + 1) {
                continue;
            }

            int pushed = dfs(v, Math.min(minC, c - f));
            if (pushed > 0) {
                edge.f += pushed;
                edges.get(e ^ 1).f -= pushed;
                return pushed;
            }
        }
        return 0;
    }

    static double maxFlow() {
        double ans = 0;
        while (bfs()) {
            Arrays.fill(p, 0);
            int curFlow;
            while ((curFlow = dfs(s, INF)) > 0) {
                ans += curFlow;
            }
        }
        return ans;
    }

    static void createPath(int u, List<Integer> path) {
        path.add(u);
        if (u == t) {
            return;
        }
        for (int e : graph.get(u)) {
            Edge edge = edges.get(e);
            int v = edge.v;
            int f = edge.f;
            int c = edge.c;

            if (f != c || c == 0) {
                continue;
            }
            edges.get(e).f = 0;
            edges.get(e ^ 1).f = 0;
            createPath(v, path);
            break;
        }
    }
}
