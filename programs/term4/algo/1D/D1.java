import java.io.*;
import java.util.*;

public class D1 {

    static class Vertex {
        int x, y;

        Vertex(int x, int y) {
            this.x = x;
            this.y = y;
        }
    }

    static class Order {
        int time;
        Vertex start;
        Vertex finish;

        Order(int time, Vertex start, Vertex finish) {
            this.time = time;
            this.start = start;
            this.finish = finish;
        }
    }
    static List<List<Integer>> edges = new ArrayList<>();
    static List<Integer> r = new ArrayList<>();
    static List<Boolean> used = new ArrayList<>();
    static List<Order> orders = new ArrayList<>();

    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));

        int n = Integer.parseInt(reader.readLine());

        int prev = 0;
        int k = 0;

        for (int i = 0; i < n; i++) {
            String[] lst = reader.readLine().split(" ");
            String[] timeParts = lst[0].split(":");
            int hh = Integer.parseInt(timeParts[0]);
            int mm = Integer.parseInt(timeParts[1]);
            int a = Integer.parseInt(lst[1]);
            int b = Integer.parseInt(lst[2]);
            int c = Integer.parseInt(lst[3]);
            int d = Integer.parseInt(lst[4]);

            int time = k + 60 * hh + mm;
            k += (time < prev) ? 24 * 60 : 0;

            orders.add(new Order(k + 60 * hh + mm, new Vertex(a, b), new Vertex(c, d)));
            prev = time;
        }

        reader.close();

        for (int i = 0; i < n; i++) {
            edges.add(new ArrayList<>());
        }

        for (int i = 0; i < n; i++) {
            for (int j = i + 1; j < n; j++) {
                if (orders.get(i).time + dist(orders.get(i).start, orders.get(i).finish) +
                        dist(orders.get(i).finish, orders.get(j).start) < orders.get(j).time) {
                    edges.get(i).add(j);
                }
            }
        }

        for (int i = 0; i < n; i++) {
            r.add(-1);
            used.add(false);
        }

        for (int i = 0; i < n; i++) {
            if (dfs(i)) {
                Collections.fill(used, false);
            }
        }

        Collections.fill(used, false);
        int ans = 0;
        for (int i = n - 1; i >= 0; i--) {
            if (dfs2(i)) {
                ans++;
            }
        }

        System.out.println(ans);
    }

    static int dist(Vertex a, Vertex b) {
        return Math.abs(a.x - b.x) + Math.abs(a.y - b.y);
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

    static boolean dfs2(int i) {
        if (used.get(i)) {
            return false;
        }
        used.set(i, true);

        if (r.get(i) != -1) {
            dfs2(r.get(i));
        }
        return true;
    }
}
