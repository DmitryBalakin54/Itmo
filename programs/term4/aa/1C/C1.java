import java.io.*;
import java.util.*;

public class C1 {
    static List<List<Integer>> edges;
    static List<Integer> r;
    static List<Boolean> used;
    static boolean[][] isEmpty;
    static int n, m;

    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        String[] firstLine = reader.readLine().split(" ");

        n = Integer.parseInt(firstLine[0]);
        m = Integer.parseInt(firstLine[1]);
        int a = Integer.parseInt(firstLine[2]);
        int b = Integer.parseInt(firstLine[3]);

        isEmpty = new boolean[n][m];
        int cnt = 0;

        for (int i = 0; i < n; i++) {
            String line = reader.readLine();
            for (int j = 0; j < m; j++) {
                if (line.charAt(j) == '*') {
                    isEmpty[i][j] = true;
                    cnt++;
                }
            }
        }

        reader.close();

        if (2 * b <= a) {
            System.out.println(b * cnt);
            return;
        }

        int k = n * m;
        edges = new ArrayList<>(k);
        for (int i = 0; i < k; i++) {
            edges.add(new ArrayList<>());
        }

        for (int i = 0; i < n; i++) {
            for (int j = 0; j < m; j++) {
                if (!isEmpty[i][j] || (i + j) % 2 == 0) {
                    continue;
                }
                if (j > 0 && isEmpty[i][j - 1]) {
                    edges.get(get(i, j)).add(get(i, j - 1));
                }
                if (j < m - 1 && isEmpty[i][j + 1]) {
                    edges.get(get(i, j)).add(get(i, j + 1));
                }
                if (i > 0 && isEmpty[i - 1][j]) {
                    edges.get(get(i, j)).add(get(i - 1, j));
                }
                if (i < n - 1 && isEmpty[i + 1][j]) {
                    edges.get(get(i, j)).add(get(i + 1, j));
                }
            }
        }

        r = new ArrayList<>(Collections.nCopies(k, -1));
        used = new ArrayList<>(Collections.nCopies(k, false));

        for (int i = 0; i < k; i++) {
            if (dfs(i)) {
                Collections.fill(used, false);
            }
        }

        int ans = 0;
        for (int j : r) {
            ans += (j != -1) ? 1 : 0;
        }

        System.out.println(ans * a + (cnt - 2 * ans) * b);
    }

    static boolean dfs(int i) {
        if (used.get(i)) {
            return false;
        }
        used.set(i, true);

        for (int j : edges.get(i)) {
            if (r.get(j) == -1 || dfs(r.get(j))) {
                r.set(j, i);
                return true;
            }
        }

        return false;
    }

    static int get(int i, int j) {
        return ((i * m + j) / 2);
    }
}
