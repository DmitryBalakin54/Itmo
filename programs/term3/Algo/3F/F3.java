import java.util.*;

public class F3 {
    static class Pair {
        int a, b;

        Pair(int a, int b) {
            this.a = a;
            this.b = b;
        }
    }

    static boolean[] was;
    static List<List<Pair>> arr;
    static int[] up;
    static int[] vals;
    static int[] cols;

    static int val = 0;
    static int maxCol = 0;

    static void dfs(int v, int p) {
        was[v] = true;
        vals[v] = val++;
        up[v] = vals[v];
        for (Pair el : arr.get(v)) {
            if (el == null) {
                continue;
            }
            if (el.a == p) {
                continue;
            }
            if (!was[el.a]) {
                dfs(el.a, v);
                up[v] = Math.min(up[v], up[el.a]);
            } else {
                up[v] = Math.min(up[v], vals[el.a]);
            }
        }
    }

    static void dfs2(int v, int p, int c) {
        was[v] = true;

        for (Pair el : arr.get(v)) {
            if (el == null) {
                continue;
            }
            if (el.a == p) {
                continue;
            }
            if (!was[el.a]) {
                if (up[el.a] >= vals[v]) {
                    maxCol++;
                    cols[el.b] = maxCol;
                    dfs2(el.a, v, maxCol);
                } else {
                    cols[el.b] = c;
                    dfs2(el.a, v, c);
                }
            } else if (cols[el.b] == 0) {
                cols[el.b] = c;
            }
        }
    }

    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);
        int n = scan.nextInt();
        int m = scan.nextInt();
        arr = new ArrayList<>();
        for (int i = 0; i < n; i++) {
            arr.add(new ArrayList<>());
        }
        was = new boolean[n];
        Arrays.fill(was, false);
        vals = new int[n];
        Arrays.fill(vals, 1000000);
        up = new int[n];
        Arrays.fill(up, 1000000);
        cols = new int[m];

        for (int i = 0; i < m; i++) {
            int a = scan.nextInt() - 1;
            int b = scan.nextInt() - 1;
            arr.get(a).add(new Pair(b, i));
            arr.get(b).add(new Pair(a, i));
        }

        for (int i = 0; i < n; i++) {
            if (!was[i]) {
                dfs(i, -1);
            }
        }

        was = new boolean[n];
        Arrays.fill(was, false);
        for (int i = 0; i < n; i++) {
            if (!was[i]) {
                maxCol++;
                dfs2(i, -1, maxCol);
            }
        }

        Map<Integer, Integer> map = new HashMap<>();
        int count = 1;
        System.out.println(maxCol - 1);
        for (int i : cols) {
            if (map.get(i) == null) {
                map.put(i, count++);
            }
            System.out.print(map.get(i) + " ");
        }
    }
}
