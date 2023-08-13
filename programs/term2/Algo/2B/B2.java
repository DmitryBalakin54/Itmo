import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class B2 {


    public static void main(String[] args) throws IOException {


        int a = 49829;
        int b = 13872439;
        int c = 23762836;
        int d = 235268;
        int e = 999837;
        int x = 12;//123;
        System.out.println((a ^ x) + (b ^ x) + (c ^ x) + (d ^ x) + (e ^ x));
        System.out.println((a + b + c + d + e + 4 * x) ^ (x));

        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        int n = Integer.parseInt(reader.readLine());

        int[] array = new int[n];
        String[] nums = reader.readLine().split(" ");
        for (int i = 0; i < n; i++) {
            array[i] = Integer.parseInt(nums[i]);
        }

        XorTree tree = new XorTree(array);

        int m = Integer.parseInt(reader.readLine());
        for (int i = 0; i < m; i++) {

            nums = reader.readLine().split(" ");
            int  op = Integer.parseInt(nums[0]);
            if (op == 1) {
                System.out.println(tree.getSum(Integer.parseInt(nums[1]), Integer.parseInt(nums[2])) + " ");
            } else if (op == 2) {
                tree.xor(Integer.parseInt(nums[1]), Integer.parseInt(nums[2]), Integer.parseInt(nums[3]));
            }
            //System.err.println(tree);
        }

        reader.close();
    }





    static class XorTree {

        final int[] array;
        final int[] xor;

        XorTree(int[] array) {
            int size = 1;
            while (size < array.length) {
                size <<= 1;
            }
            this.array = new int[2 * size - 1];
            this.xor = new int[2 * size - 1];
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
                //return Pair.copyOf(array[index]);
                if (xor[index] == 0 || isChild(index)) {
                    return array[index];
                }
            }
            push(index);
            int mid = (left + right) / 2;
            int leftSon = getSum(2 * index + 1, left, mid, qLeft, qRight);
            int rightSon = getSum(2 * index + 2, mid + 1, right, qLeft, qRight);
            return leftSon + rightSon;

        }



        public void xor(int qLeft, int qRight, int value) {
            xor(0, array.length / 2, array.length - 1, qLeft - 1 + array.length / 2, qRight - 1 + array.length / 2, value);
        }

        private void xor(int  index, int left, int right, int qLeft, int qRight, int value) {
            if (right < qLeft || qRight < left) {
                return;
            }

            if (qLeft <= left && right <= qRight) {
                //return Pair.copyOf(array[index]);
                xor[index] ^= value;
                array[index] ^= value;
                return;
            }
            push(index);
            int mid = (left + right) / 2;
            xor(2 * index + 1, left, mid, qLeft, qRight, value);
            xor(2 * index + 2, mid + 1, right, qLeft, qRight, value);
            array[index] = array[2 * index + 1] + array[2 * index + 2];
        }

        private void push(int index) {
            if (2 * index + 1 >= array.length) {
                return;
            }

            array[2 * index + 1] ^= xor[index];
            xor[2 * index + 1] ^= xor[index];


            array[2 * index + 2] ^= xor[index];
            xor[2 * index + 2] ^= xor[index];


            xor[index] = 0;
        }

        @Override
        public String toString() {
            StringBuilder res = new StringBuilder();

            int k = 1;
            int two = 1;
            for (int i = 0; i < array.length; i++) {
                res.append("(" + array[i] + ", " + xor[i] + ")");

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

        private boolean isChild(int index) {
            return array.length / 2 <= index && index <= array.length - 1;
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
