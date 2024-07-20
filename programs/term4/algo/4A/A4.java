import java.util.*;



public class A4 {
    static final int INF = Integer.MAX_VALUE;
    static class Edge {
        int u, v, f, c, w;

        Edge(int u, int v, int c, int w) {
            this.u = u;
            this.v = v;
            this.f = 0;
            this.c = c;
            this.w = w;
        }
    }

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        int n = scanner.nextInt();
        int m = scanner.nextInt();

        List<List<Integer>> graph = new ArrayList<>();
        for (int i = 0; i < n; i++) {
            graph.add(new ArrayList<>());
        }

        List<Edge> edges = new ArrayList<>();

        for (int i = 0; i < m; i++) {
            int u = scanner.nextInt();
            int v = scanner.nextInt();
            int c = scanner.nextInt();
            int s = scanner.nextInt();
            addEdges(u - 1, v - 1, c, s, graph, edges);
        }

        System.out.println(minCost(0, n - 1, graph, edges));
        scanner.close();
    }

    static boolean existsPath(int s, int t, List<List<Integer>> graph, List<Edge> edges, int[] p) {
        int n = graph.size();
        int[] d = new int[n];
        Arrays.fill(d, INF);
        TreeSet<Pair> relax = new TreeSet<>();
        relax.add(new Pair(0, s));
        d[s] = 0;

        while (!relax.isEmpty()) {
            Pair current = relax.pollFirst();
            int dist = current.first;
            int u = current.second;

            for (int e : graph.get(u)) {
                Edge edge = edges.get(e);
                int v = edge.v;
                int f = edge.f;
                int c = edge.c;
                int w = edge.w;

                if (d[v] <= d[u] + w || c - f == 0) {
                    continue;
                }

                if (d[v] != INF) {
                    relax.remove(new Pair(d[v], v));
                }

                d[v] = d[u] + w;
                p[v] = e;
                relax.add(new Pair(d[v], v));
            }
        }

        return d[t] != INF;
    }

    static int getPathMin(int s, int v, List<Edge> edges, int[] p) {
        if (v == s) {
            return INF;
        }
        Edge edge = edges.get(p[v]);
        return Math.min(edge.c - edge.f, getPathMin(s, edge.u, edges, p));
    }

    static void addEdges(int u, int v, int c, int s, List<List<Integer>> graph, List<Edge> edges) {
        graph.get(u).add(edges.size());
        edges.add(new Edge(u, v, c, s));
        graph.get(v).add(edges.size());
        edges.add(new Edge(v, u, 0, -s));
    }

    static int getReverse(int e) {
        return e + (e % 2 == 0 ? 1 : -1);
    }

    static long minCost(int s, int t, List<List<Integer>> graph, List<Edge> edges) {
        int[] p = new int[graph.size()];
        long result = 0;

        while (existsPath(s, t, graph, edges, p)) {
            long curFlow = getPathMin(s, t, edges, p);

            for (int cur = t; cur != s; cur = edges.get(p[cur]).u) {
                int e = p[cur];
                edges.get(e).f += curFlow;
                edges.get(getReverse(e)).f -= curFlow;
                result += curFlow * edges.get(e).w;
            }
        }
        return result;
    }
}

class Pair implements Comparable<Pair> {
    int first, second;

    Pair(int first, int second) {
        this.first = first;
        this.second = second;
    }

    @Override
    public int compareTo(Pair o) {
        if (this.first != o.first) {
            return Integer.compare(this.first, o.first);
        } else {
            return Integer.compare(this.second, o.second);
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;
        Pair pair = (Pair) obj;
        return first == pair.first && second == pair.second;
    }

    @Override
    public int hashCode() {
        return Objects.hash(first, second);
    }
}
