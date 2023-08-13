import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class C2 {


    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        int n = Integer.parseInt(reader.readLine());

        int[] array = new int[n];
        String[] nums = reader.readLine().split(" ");
        for (int i = 0; i < n; i++) {
            array[i] = Integer.parseInt(nums[i]);
        }

        SumTree tree = new SumTree(array);

        int m = Integer.parseInt(reader.readLine());
        for (int i = 0; i < m; i++) {

            nums = reader.readLine().split(" ");
            char ch = nums[0].charAt(0);
            if (ch == 'm') {
                System.out.print(tree.getSum(Integer.parseInt(nums[1]), Integer.parseInt(nums[2])) + " ");
            } else if (ch == 'a') {
                tree.add(Integer.parseInt(nums[1]), Integer.parseInt(nums[2]), Integer.parseInt(nums[3]));
            }
            //System.err.println(tree);
        }

        reader.close();
    }





    static class SumTree {

        final int[] array;
        final int[] add;
        final int[] len;

        SumTree(int[] array) {
            int size = 1;
            while (size < array.length) {
                size <<= 1;
            }
            this.array = new int[2 * size - 1];
            this.add = new int[2 * size - 1];
            this.len = new int[2 * size - 1];
            for (int i = size - 1; i < size - 1 + array.length; i++) {
                this.array[i] = array[i - size + 1];
            }
            for (int i = size - 1 + array.length; i < this.array.length; i++) {
                this.array[i] = 0;
            }
            build();
        }

        private void build() {
            for (int i = array.length / 2 - 1; i >= 0; i--) {
                array[i] = build(i);
            }
        }

        private int build(int index) {
            return array[index] = array[2 * index + 1] + array[2 * index + 2];
        }

        public int getSum(int qLeft, int qRight) {
            return getSum(0, array.length / 2, array.length - 1, qLeft - 1 + array.length / 2, qRight - 1 + array.length / 2);
        }

        private int getSum(int index, int left, int right, int qLeft, int qRight) {
            if (right < qLeft || qRight < left) {
                return 0;
            }

            if (qLeft <= left && right <= qRight) {
                return array[index];
            }
            push(index);
            int mid = (left + right) / 2;
            int leftSon = getSum(2 * index + 1, left, mid, qLeft, qRight);
            int rightSon = getSum(2 * index + 2, mid + 1, right, qLeft, qRight);
            return leftSon + rightSon;

        }



        public void add(int qLeft, int qRight, int value) {
            add(0, array.length / 2, array.length - 1, qLeft - 1 + array.length / 2, qRight - 1 + array.length / 2, value);
        }

        private void add(int  index, int left, int right, int qLeft, int qRight, int value) {
            if (right < qLeft || qRight < left) {
                return;
            }

            if (qLeft <= left && right <= qRight) {
                //return Pair.copyOf(array[index]);
                add[index] += value;
                array[index] += value;
                return;
            }
            push(index);
            int mid = (left + right) / 2;
            add(2 * index + 1, left, mid, qLeft, qRight, value);
            add(2 * index + 2, mid + 1, right, qLeft, qRight, value);
            array[index] = Math.max(array[2 * index + 1], array[2 * index + 2]);
        }

        private void push(int index) {
            if (2 * index + 1 >= array.length) {
                return;
            }

            array[2 * index + 1] += add[index];
            add[2 * index + 1] += add[index];


            array[2 * index + 2] += add[index];
            add[2 * index + 2] += add[index];


            add[index] = 0;
        }

        @Override
        public String toString() {
            StringBuilder res = new StringBuilder();

            int k = 1;
            int two = 1;
            for (int i = 0; i < array.length; i++) {
                res.append("(" + array[i] + ", " + add[i] + ")");

                if (i == two - 1) {
                    k = k << 1;
                    two += k;
                    res.append('\n');
                } else {
                    res.append(" ");
                }

            }
            return res.toString();
        }
    }

    static class Pair {
        int value;
        final int index;

        int add;

        public Pair(int value, int index) {
            this.value = value;
            this.index = index;
            this.add = 0;
        }

        static Pair copyOf(Pair p) {
            return new Pair(p.value, p.index);
        }

        static Pair max(Pair p1, Pair p2) {
            if (p1 == null && p2 == null) {
                return null;
            } else if (p1 == null) {
                //return copyOf(p2);
                return p2;
            } else if (p2 == null) {
                //return copyOf(p1);
                return p1;
            }


            if (p1.value > p2.value) {
                //return copyOf(p1);
                return p1;
            } else {
                //return copyOf(p2);
                return p2;
            }
        }

        @Override
        public String toString() {
            return "(" + value + ", " + index + " ," + add + ")";
        }
    }
}
