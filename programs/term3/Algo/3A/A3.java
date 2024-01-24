import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.StringTokenizer;

public class A3 {
    static class Edge {
        int e, n;

        Edge(int end, int num) {
            e = end;
            n = num;
        }
    }

    static List<Boolean> was;
    static List<List<Edge>> e;
    static List<Integer> up;
    static List<Integer> val;
    static List<Integer> bridges;
    static int l = 0;

    static void dfs(int v, int p) {
        was.set(v, true);
        val.set(v, l++);
        up.set(v, val.get(v));

        for (Edge ed : e.get(v)) {
            if (!was.get(ed.e)) {
                dfs(ed.e, ed.n);
            }

            if (ed.n != p) {
                up.set(v, Math.min(up.get(v), up.get(ed.e)));
            }
        }

        if (up.get(v).equals(val.get(v)) && p != -1) {
            bridges.add(p);
        }
    }

    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        StringTokenizer tokenizer = new StringTokenizer(reader.readLine());

        int n = Integer.parseInt(tokenizer.nextToken());
        int m = Integer.parseInt(tokenizer.nextToken());

        e = new ArrayList<>(n);
        for (int i = 0; i < n; ++i) {
            e.add(new ArrayList<>());
        }

        was = new ArrayList<>(Collections.nCopies(n, false));
        val = new ArrayList<>(Collections.nCopies(n, 1000000));
        up = new ArrayList<>(Collections.nCopies(n, 1000000));
        bridges = new ArrayList<>();

        for (int i = 0; i < m; ++i) {
            tokenizer = new StringTokenizer(reader.readLine());
            int a = Integer.parseInt(tokenizer.nextToken()) - 1;
            int b = Integer.parseInt(tokenizer.nextToken()) - 1;
            e.get(a).add(new Edge(b, i));
            e.get(b).add(new Edge(a, i));
        }

        for (int i = 0; i < n; ++i) {
            if (!was.get(i)) {
                dfs(i, -1);
            }
        }

        System.out.println(bridges.size());
        Collections.sort(bridges);
        for (int b : bridges) {
            System.out.print(b + 1 + " ");
        }
    }
}
