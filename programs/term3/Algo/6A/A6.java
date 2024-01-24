import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.*;

public class A6 {
    static final int S = 10000;

    static int ds(int[] c1, int[] c2) {
        return (c1[0] - c2[0]) * (c1[0] - c2[0]) + (c1[1] - c2[1]) * (c1[1] - c2[1]);
    }

    static int ct(int n, int[][] c, int s, int e) {
        int[][] sc = new int[n][2];
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < 2; j++) {
                sc[i][j] = c[i][j] + S;
            }
        }

        Map<Integer, List<int[]>> g = new HashMap<>();
        for (int i = 0; i < n; i++) {
            List<int[]> nb = new ArrayList<>();
            for (int j = 0; j < n; j++) {
                if (j != i) {
                    nb.add(new int[]{j + 1, ds(sc[i], sc[j])});
                }
            }
            g.put(i + 1, nb);
        }

        int[] d = new int[n + 1];
        Arrays.fill(d, Integer.MAX_VALUE);
        d[s] = 0;

        PriorityQueue<int[]> h = new PriorityQueue<>(Comparator.comparingInt(a -> a[0]));
        h.offer(new int[]{0, s});

        Set<Integer> p = new HashSet<>();

        while (!h.isEmpty()) {
            int[] ce = h.poll();
            int cd = ce[0];
            int cc = ce[1];

            if (p.contains(cc)) {
                continue;
            }

            p.add(cc);

            g.getOrDefault(cc, Collections.emptyList()).forEach(nbr -> {
                int ne = nbr[0];
                int w = nbr[1];
                int nd = cd + w;

                if (nd < d[ne]) {
                    h.offer(new int[]{nd, ne});
                    d[ne] = nd;
                }
            });
        }

        return d[e];
    }

    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));

        int n = Integer.parseInt(reader.readLine());
        int[][] c = new int[n][2];
        for (int i = 0; i < n; i++) {
            String[] tokens = reader.readLine().split(" ");
            c[i][0] = Integer.parseInt(tokens[0]);
            c[i][1] = Integer.parseInt(tokens[1]);
        }

        String[] se = reader.readLine().split(" ");
        int s = Integer.parseInt(se[0]);
        int e = Integer.parseInt(se[1]);

        int r = ct(n, c, s, e);
        System.out.println(r);
    }
}
