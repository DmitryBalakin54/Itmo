import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Scanner;

public class F1 {
    static class Graph {

        private final List<List<Integer>> lst;

        public Graph(int n) {
            lst = new ArrayList<>(n);
            for (int i = 0; i < n; i++) {
                lst.add(new ArrayList<>());
            }
        }

        public int len() {
            return lst.size();
        }

        public void add(int i, int j) {
            lst.get(i).add(j);
        }

        public List<Integer> topSort() {
            List<Integer> clrs = new ArrayList<>(Collections.nCopies(len(), 0));
            List<Integer> res = new ArrayList<>();
            boolean is = true;

            for (int d = 0; d < len(); d++) {
                is &= dfs(d, clrs, res);
            }

            if (!is) {
                res.clear();
            } else {
                Collections.reverse(res);
            }

            return res;
        }

        private boolean dfs(int vertex, List<Integer> colors, List<Integer> result) {
            boolean is = true;

            if (colors.get(vertex) == 0) {
                colors.set(vertex, 1);

                for (int neighbor : lst.get(vertex)) {
                    is &= dfs(neighbor, colors, result);
                }

                colors.set(vertex, 2);
                result.add(vertex);
            } else if (colors.get(vertex) == 1) {
                is = false;
            }

            return is;
        }
    }


    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);

        int n = scan.nextInt();
        int m = scan.nextInt();

        Graph graph = new Graph(n);

        for (int i = 0; i < m; i++) {
            int a = scan.nextInt();
            int b = scan.nextInt();
            graph.add(a - 1, b - 1);
        }

        List<Integer> topSortResult = graph.topSort();

        if (topSortResult.isEmpty()) {
            System.out.println(-1);
        } else {
            for (int i : topSortResult) {
                System.out.print(i + 1 + " ");
            }
        }
    }
}
