import java.util.*;

public class B4 {
    static final long EDGE = 1_000_000_000_000L;

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        int n = sc.nextInt();
        sc.nextLine();

        int[][] c = new int[n + 1][n + 1];
        for (int i = 1; i <= n; i++) {
            c[i][0] = 0;
            for (int j = 1; j <= n; j++) {
                c[i][j] = sc.nextInt();
            }
            if (sc.hasNextLine()) {
                sc.nextLine();
            }
        }

        long[] row_w = new long[n + 1];
        long[] col_w = new long[n + 1];
        int[] p = new int[n + 1];
        int[] way = new int[n + 1];

        for (int i = 1; i <= n; i++) {
            p[0] = i;
            int j0 = 0;
            long[] minByCol = new long[n + 1];
            Arrays.fill(minByCol, EDGE);
            boolean[] mark = new boolean[n + 1];
            do {
                mark[j0] = true;
                int i0 = p[j0];
                long delta = EDGE;
                int j1 = 0;
                for (int j = 1; j <= n; j++) {
                    if (!mark[j]) {
                        long cur = c[i0][j] - row_w[i0] - col_w[j];
                        if (cur < minByCol[j]) {
                            minByCol[j] = cur;
                            way[j] = j0;
                        }
                        if (minByCol[j] < delta) {
                            delta = minByCol[j];
                            j1 = j;
                        }
                    }
                }
                for (int j = 0; j <= n; j++) {
                    if (mark[j]) {
                        row_w[p[j]] += delta;
                        col_w[j] -= delta;
                    } else {
                        minByCol[j] -= delta;
                    }
                }
                j0 = j1;
            } while (p[j0] != 0);
            do {
                int j1 = way[j0];
                p[j0] = p[j1];
                j0 = j1;
            } while (j0 != 0);
        }

        System.out.println(-col_w[0]);
        for (int j = 1; j <= n; j++) {
            System.out.println(p[j] + " " + j);
        }
        sc.close();
    }
}
