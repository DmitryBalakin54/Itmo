import java.util.*;

public class F32 {
    static class Pair {
        int a;
        int b;

        Pair(int a, int b) {
            this.a = a;
            this.b = b;
        }
    }

    static List<List<Pair>> g;
    static boolean[] was;
    static int[] startTime;
    static int[] data;
    static int[] cols;
    static Stack<Integer> stack;

    static int time = 0;
    static int maxCol = 1;

    static void paint(int u, int n) {
        startTime[u] = ++time;
        data[u] = time;
        was[u] = true;
        int cnt = 0;
        for (Pair e : g.get(u)) {
            int v = e.a;
            int num = e.b;
            if (n == num) {
                continue;
            }
            if (was[v]) {
                data[u] = Math.min(data[u], startTime[v]);
                if (startTime[v] < startTime[u]) {
                    stack.push(num);
                }
                continue;
            }
            cnt++;
            stack.push(num);
            paint(v, num);
            data[u] = Math.min(data[v], data[u]);
            if ((n == -1 && cnt > 1) || (n != -1 && data[v] >= startTime[u]) ) {
                int cur_color = maxCol;
                while (num != stack.peek()) {
                    cols[stack.peek()] = cur_color;
                    stack.pop();
                }
                maxCol++;
                cols[num] = cur_color;
                stack.pop();
            }
        }
    }

    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);
        int n = scan.nextInt();
        int m = scan.nextInt();
        g = new ArrayList<>();
        for (int i = 0; i < n; i++) {
            g.add(new ArrayList<>());
        }
        was = new boolean[n];
        Arrays.fill(was, false);
        startTime = new int[n];
        data = new int[n];
        cols = new int[m];
        stack = new Stack<>();

        for (int i = 0; i < m; i++) {
            int u = scan.nextInt() - 1;
            int v = scan.nextInt() - 1;
            g.get(u).add(new Pair(v, i));
            g.get(v).add(new Pair(u, i));
        }

        for (int i = 0; i < n; i++) {
            if (!was[i]) {
                paint(i, -1);
                boolean f = !stack.empty();
                while (!stack.empty()) {
                    cols[stack.peek()] = maxCol;
                    stack.pop();
                }
                if (f) {
                    maxCol++;
                }
            }
        }
        System.out.println(maxCol - 1);
        Set<Integer> set = new HashSet<>();
        Map<Integer, Integer> map = new HashMap<>();
        int curCol = 1;
        for (int i = 0; i < n; i++) {
            if (!set.contains(cols[i])) {
                set.add(cols[i]);
                map.put(cols[i], curCol++);
            }
        }
        for (int c : cols) {
            System.out.print(map.get(c) + " ");
        }
    }
}
