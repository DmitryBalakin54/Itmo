import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class I1 {
    static List<List<Pair<Integer, Integer>>> graph;
    static boolean[] was;
    static int[] dd;
    static int n, m, s, t;

    static class Pair<F, S> {
        F first;
        S second;

        Pair(F first, S second) {
            this.first = first;
            this.second = second;
        }
    }

    static void dfs(int i) {
        was[i] = true;
        for (Pair<Integer, Integer> j : graph.get(i)) {
            if (!was[j.first]) {
                dfs(j.first);
            }

            dd[i] = Math.min(dd[i], dd[j.first] + j.second);
        }
    }

    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);

        n = scan.nextInt();
        m = scan.nextInt();
        s = scan.nextInt() - 1;
        t = scan.nextInt() - 1;

        graph = new ArrayList<>(n);
        for (int i = 0; i < n; ++i) {
            graph.add(new ArrayList<>());
        }

        was = new boolean[n];
        dd = new int[n];
        for (int i = 0; i < n; ++i) {
            dd[i] = Integer.MAX_VALUE - 10000;
        }
        dd[s] = 0;

        int a, b, w;
        for (int i = 0; i < m; ++i) {
            a = scan.nextInt() - 1;
            b = scan.nextInt() - 1;
            w = scan.nextInt();
            graph.get(b).add(new Pair<>(a, w));
        }

        dfs(t);

        if (!was[s]) {
            System.out.println("Unreachable");
        } else {
            System.out.println(dd[t]);
        }
    }
}
