import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.*;

public class B6 {
    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));

        String[] s;
        int n = Integer.parseInt(reader.readLine());

        s = reader.readLine().split(" ");
        int[] values = new int[n];
        for (int i = 0; i < n; i ++) {
            values[i] = Integer.parseInt(s[i]);
        }


        Tree tree = new Tree(0, values[0], n);
        long numOfRoot = ((long) (1 + n) * n) / 2;
        for (int i = 1; i < n; i++) {
            s = reader.readLine().split(" ");
            int a = Integer.parseInt(s[0]);
            int b = Integer.parseInt(s[1]);
            numOfRoot -= a;
            tree.add(a - 1, values[a - 1],  b - 1);
        }
        tree.changeRoot((int)numOfRoot - 1, values[(int)numOfRoot - 1]);
        tree.dfs();

        int m = Integer.parseInt(reader.readLine());
        for (int i = 0; i < m; i++) {
            s = reader.readLine().split(" ");
            System.out.println(tree.lcaSum(Integer.parseInt(s[0]) - 1, Integer.parseInt(s[1]) - 1));
        }


        reader.close();
    }


    static class Tree {
        int root;

        Node[] data;

        ArrayList<ArrayList<Integer>> jumps;
        ArrayList<ArrayList<Long>> sums;
        public Tree(int indexOfRoot, int valueOFRoot, int size) {
            root = indexOfRoot;
            data = new Node[size];
            data[0] = new Node(indexOfRoot, 0, valueOFRoot, 0);
            for (int i = 1; i < data.length; i++) {
                data[i] = new Node(i, 0, 0, 0);
            }
        }

        public void add(int index, int value, int indexOfFather) {
            data[index].indexOfFather = indexOfFather;
            data[index].value = value;
            data[indexOfFather].children.add(data[index]);
        }

        public void changeRoot(int index, int value) {
            if (root != index) {
                root = index;
                data[root].value = value;
            }
        }


        public void dfs() {
            jumps = new ArrayList<>();
            for (int i = 0; i < data.length; i++) {
                jumps.add(new ArrayList<>());
            }

            sums = new ArrayList<>();
            for (int i = 0; i < data.length; i++) {
                sums.add(new ArrayList<>());
            }

            dfs(root);
        }

        private void dfs(int index) {

            if (index != root) {
                jumps.get(index).add(data[index].indexOfFather);
                sums.get(index).add((long) data[data[index].indexOfFather].value);
                int log = (int) (Math.log(data[index].height) / Math.log(2));
                for (int i = 1; i <= log; i++) {
                    int ind = index;
                    long sum = 0;
                    sum += sums.get(ind).get(i - 1);
                    ind = jumps.get(ind).get(i - 1);
                    sum += sums.get(ind).get(i - 1);
                    ind = jumps.get(ind).get(i - 1);
                    jumps.get(index).add(ind);
                    sums.get(index).add(sum);
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

        public long lcaSum(int ind1, int ind2) {
            if (ind1 == ind2) {
                return 0;
            }

            int lower = ind1;
            int upper = ind2;

            long sum = 0;

            if (data[lower].height < data[upper].height) {
                int tmp = lower;
                lower = upper;
                upper = tmp;
            }

            while (data[lower].height > data[upper].height) {
                int jumpSize = data[lower].height - data[upper].height;
                int log = (int) (Math.log(jumpSize) / Math.log(2));
                sum += sums.get(lower).get(log);
                lower = data[jumps.get(lower).get(log)].index;
            }

            while (lower != upper) {
                int log = (int) (Math.log(data[lower].height) / Math.log(2));
                int jump = log;
                while (data[jumps.get(lower).get(jump)] == data[jumps.get(upper).get(jump)] && jump > 0) {
                    jump--;
                }
                sum += sums.get(lower).get(jump);
                sum += sums.get(upper).get(jump);
                lower = data[jumps.get(lower).get(jump)].index;
                upper = data[jumps.get(upper).get(jump)].index;
            }

            return sum + data[ind1].value + data[ind2].value - data[lower].value;
        }

    }

    static class Node {

        int index;
        int height;
        int indexOfFather;
        List<Node> children;

        int value;

        public Node(int index, int height, int value) {
            this.index = index;
            this.height = height;
            this.value = value;
            this.children = new ArrayList<>();
        }

        public Node(int index, int height, int value, int indexOfFather) {
            this(index, height, value);
            this.indexOfFather = indexOfFather;
        }

        public void addChild(Node child) {
            children.add(child);
        }
    }
}
