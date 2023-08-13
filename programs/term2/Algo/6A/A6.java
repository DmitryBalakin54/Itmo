import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.*;

public class A6 {
    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));

        String[] s = reader.readLine().split(" ");
        int n = Integer.parseInt(s[0]);
        int m = Integer.parseInt(s[1]);

        Tree tree = new Tree(0, n);

        s = reader.readLine().split(" ");
        for (int i = 1; i < n; i++) {
            tree.add(i, Integer.parseInt(s[i - 1]));
        }
        tree.dfs();

        s = reader.readLine().split(" ");
        int a1 = Integer.parseInt(s[0]);
        int a2 = Integer.parseInt(s[1]);

        s = reader.readLine().split(" ");

        reader.close();

        int x = Integer.parseInt(s[0]);
        int y = Integer.parseInt(s[1]);
        int z = Integer.parseInt(s[2]);


        int v = tree.lca(a1, a2);
        long res = 0;
        res += v;
        for (int i = 0; i < m - 1; i++) {
            a1 = (int)(((long) x * a1 + (long) y * a2 + z) % n);
            a2 = (int)(((long) x * a2 + (long) y * a1 + z) % n);
            v = tree.lca((a1 + v) % n, a2);
            res += v;
        }


        System.out.println(res);
    }


    static class Tree {
        Node root;

        Node[] data;

        ArrayList<ArrayList<Integer>> jumps;
        public Tree(int indexOfRoot, int size) {
            root = new Node(indexOfRoot, 0);
            data = new Node[size];
            data[0] = root;
            for (int i = 1; i < data.length; i++) {
                data[i] = new Node(i, 0, 0);
            }
        }

        public void add(int index, int indexOfFather) {
            data[index].indexOfFather = indexOfFather;
            data[indexOfFather].children.add(data[index]);
        }


        public void dfs() {
            jumps = new ArrayList<>();
            for (int i = 0; i < data.length; i++) {
                jumps.add(new ArrayList<>());
            }
            dfs(0);
        }

        private void dfs(int index) {
            if (index >= data.length) {
                return;
            }
            if (index != 0) {
                jumps.get(index).add(data[index].indexOfFather);
                int log = (int) (Math.log(data[index].height) / Math.log(2));
                for (int i = 1; i <= log; i++) {
                    int ind = index;
                    ind = jumps.get(ind).get(i - 1);
                    ind = jumps.get(ind).get(i - 1);
                    jumps.get(index).add(ind);
                }
            }

            for (Node i : data[index].children) {
                i.height = data[index].height + 1;
                dfs(i.index);
            }
        }

        public int lca(int ind1, int ind2) {
            int lower = ind1;
            int upper = ind2;

            if (data[lower].height < data[upper].height) {
                int tmp = lower;
                lower = upper;
                upper = tmp;
            }

            while (data[lower].height > data[upper].height) {
                int jumpSize = data[lower].height - data[upper].height;
                int log = (int) (Math.log(jumpSize) / Math.log(2));
                lower = data[jumps.get(lower).get(log)].index;
            }

            while (lower != upper) {
                int log = (int) (Math.log(data[lower].height) / Math.log(2));
                int jump = log;
                while (data[jumps.get(lower).get(jump)] == data[jumps.get(upper).get(jump)] && jump > 0) {
                    jump--;
                }
                lower = data[jumps.get(lower).get(jump)].index;
                upper = data[jumps.get(upper).get(jump)].index;
            }

            return lower;
        }

    }

    static class Node {

        int index;
        int height;
        int indexOfFather;
        List<Node> children;

        public Node(int index, int height) {
            this.index = index;
            this.height = height;
            this.children = new ArrayList<>();
        }

        public Node(int index, int height, int indexOfFather) {
            this(index, height);
            this.indexOfFather = indexOfFather;
        }

        public void addChild(Node child) {
            children.add(child);
        }
    }
}
