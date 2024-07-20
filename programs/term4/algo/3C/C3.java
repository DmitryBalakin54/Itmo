import java.util.*;

public class C3 {
    static final long BIG = 1_000_000_000_000L;
    static int n, m;
    static boolean[] mark;
    static List<List<int[]>> g;
    static int[][] edges;

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        n = sc.nextInt();
        m = sc.nextInt();

        edges = new int[m][2];
        g = new ArrayList<>(n);
        for (int i = 0; i < n; i++) {
            g.add(new ArrayList<>());
        }

        for (int i = 0; i < m; i++) {
            int aa = sc.nextInt() - 1;
            int bb = sc.nextInt() - 1;
            int cc = sc.nextInt();
            g.get(aa).add(new int[]{bb, cc, 0, g.get(bb).size()});
            edges[i][0] = aa;
            edges[i][1] = g.get(aa).size() - 1;
            g.get(bb).add(new int[]{aa, cc, 0, g.get(aa).size() - 1});
        }

        long answer = 0;
        while (true) {
            mark = new boolean[n];
            long delta = dfs(0, BIG);
            if (delta == 0) break;
            answer += delta;
        }

        mark = new boolean[n];
        dfs(0, BIG);
        List<Integer> answerList = new ArrayList<>();
        for (int i = 0; i < m; i++) {
            int[] edge = edges[i];
            if ((mark[edge[0]] && !mark[g.get(edge[0]).get(edge[1])[0]]) || (!mark[edge[0]] && mark[g.get(edge[0]).get(edge[1])[0]])) {
                answerList.add(i + 1);
            }
        }

        System.out.println(answerList.size() + " " + answer);
        for (int idx : answerList) {
            System.out.print(idx + " ");
        }
        sc.close();
    }

    static long dfs(int v, long dmin) {
        if (v == n - 1 || mark[v]) return dmin;
        mark[v] = true;
        for (int i = 0; i < g.get(v).size(); i++) {
            int[] edge = g.get(v).get(i);
            int u = edge[0];
            int cost = edge[1];
            int f = edge[2];
            int j = edge[3];
            if (!mark[u] && cost - f > 0) {
                long d = dfs(u, Math.min(dmin, cost - f));
                if (d > 0) {
                    g.get(v).get(i)[2] += d;
                    g.get(u).get(j)[2] -= d;
                    return d;
                }
            }
        }
        return 0;
    }
}
