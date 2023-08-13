import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class C1 {
    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        int n = Integer.parseInt(reader.readLine());

        int[] array = new int[n];
        String[] nums = reader.readLine().split(" ");
        for (int i = 0; i < n; i++) {
            array[i] = Integer.parseInt(nums[i]);
        }

        ZeroTree tree = new ZeroTree(array);

        int m = Integer.parseInt(reader.readLine());
        for (int i = 0; i < m; i++) {

            nums = reader.readLine().split(" ");
            String op = nums[0];
            if (op.equals("QUERY")) {
                Pair p = tree.getZeros(Integer.parseInt(nums[1]), Integer.parseInt(nums[2]));
                System.out.println(Math.max(Math.max(p.leftZeros, p.rightZeros), p.zeros));
            } else if (op.equals("UPDATE")) {
                tree.update(Integer.parseInt(nums[1]), Integer.parseInt(nums[2]));
            }
            //System.err.println(tree);
        }

        reader.close();
    }

    static class ZeroTree {

        final Pair[] array;

        ZeroTree(int[] array) {
            int size = 1;
            while (size < array.length) {
                size <<= 1;
            }
            this.array = new Pair[2 * size - 1];
            for (int i = size - 1; i < size - 1 + array.length; i++) {
                int z = array[i - size + 1] == 0 ? 1 : 0;
                this.array[i] = new Pair(array[i - size + 1], 1, z, z, z);
            }
            for (int i = size - 1 + array.length; i < this.array.length; i++) {
                this.array[i] = null;
            }
            build();
        }

        private void build() {
            for (int i = array.length / 2 - 1; i >= 0; i--) {
                build(i);
            }
        }

        private void build(int index) {
            Pair p = new Pair(0, 0);
            Pair l = array[2 * index + 1];
            Pair r =  array[2 * index + 2];

            if (l == null && r == null) {
                array[index] = null;
                return;
            }

            if (l == null) {
                l = new Pair(0, 0, 0, 0, 0);
            }
            if (r == null) {
                r = new Pair(0, 0, 0, 0, 0);
            }

            p.value = 0;
            p.count = l.count + r.count;
            int leftZeros = l.leftZeros == l.count ? l.leftZeros + r.leftZeros : l.leftZeros;
            int rightZeros = r.rightZeros == r.count ? r.rightZeros + l.rightZeros : r.rightZeros;
            int zeros = Math.max(Math.max(Math.max(Math.max(l.zeros, r.zeros), leftZeros), rightZeros), l.rightZeros + r.leftZeros);
            p.leftZeros = leftZeros;
            p.rightZeros = rightZeros;
            p.zeros = zeros;

            array[index] = p;
        }

        public Pair getZeros(int qLeft, int qRight) {
            return getZeros(0, array.length / 2, array.length - 1, qLeft - 1 + array.length / 2, qRight - 1 + array.length / 2);
        }

        private Pair getZeros(int index, int left, int right, int qLeft, int qRight) {
            if (right < qLeft || qRight < left) {
                return null;
            }

            if (qLeft <= left && right <= qRight) {
                //return Pair.copyOf(array[index]);
                return array[index];
            }

            int mid = (left + right) / 2;
            Pair leftSon = getZeros(2 * index + 1, left, mid, qLeft, qRight);
            Pair rightSon = getZeros(2 * index + 2, mid + 1, right, qLeft, qRight);

            if (leftSon == null) {
                leftSon = new Pair(0, 0, 0, 0, 0);
            }
            if (rightSon == null) {
                rightSon = new Pair(0, 0, 0, 0, 0);
            }

            int leftZeros = leftSon.leftZeros == leftSon.count ? leftSon.leftZeros + rightSon.leftZeros : leftSon.leftZeros;
            int rightZeros = rightSon.rightZeros == rightSon.count ? rightSon.rightZeros + leftSon.rightZeros : rightSon.rightZeros;
            int zeros = Math.max(Math.max(Math.max(Math.max(leftSon.zeros, rightSon.zeros), leftZeros), rightZeros), leftSon.rightZeros + rightSon.leftZeros);
            return new Pair(0, leftSon.count + rightSon.count, leftZeros, zeros, rightZeros);
        }



        void update(int index, int value) {
            index = array.length / 2 + index - 1;

            if ((value == 0 && array[index].value == 0) || (value != 0 && array[index].value != 0) ) {
                return;
            }

            int z = value == 0 ? 1 : 0;
            Pair p = array[index];
            p.value = value;
            p.leftZeros = z;
            p.zeros = z;
            p.rightZeros = z;
            index = (index - 1) / 2;

            while (true) {
                p = array[index];

                Pair l = array[2 * index + 1];
                Pair r =  array[2 * index + 2];

                if (l == null && r == null) {
                    break;
                }

                if (l == null) {
                    l = new Pair(0, 0, 0, 0, 0);
                }
                if (r == null) {
                    r = new Pair(0, 0, 0, 0, 0);
                }

                int leftZeros = l.leftZeros == l.count ? l.leftZeros + r.leftZeros : l.leftZeros;
                int rightZeros = r.rightZeros == r.count ? r.rightZeros + l.rightZeros : r.rightZeros;
                int zeros = Math.max(Math.max(Math.max(Math.max(l.zeros, r.zeros), leftZeros), rightZeros), l.rightZeros + r.leftZeros);
                p.leftZeros = leftZeros;
                p.rightZeros = rightZeros;
                p.zeros = zeros;

                if (index == 0) {
                    break;
                } else {
                    index = (index - 1) /2;
                }

            }
        }


        @Override
        public String toString() {
            StringBuilder res = new StringBuilder();

            int k = 1;
            int two = 1;
            for (int i = 0; i < array.length; i++) {
                res.append(array[i]);

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
        int count;

        int leftZeros;
        int zeros;
        int rightZeros;

        public Pair(int value, int count) {
            this.value = value;
            this.count = count;
            this.zeros = 0;
            this.leftZeros = 0;
            this.rightZeros = 0;
        }

        public Pair(int value, int count, int leftZeros, int zeros, int rightZeros) {
            this.value = value;
            this.count = count;
            this.zeros = zeros;
            this.leftZeros = leftZeros;
            this.rightZeros = rightZeros;
        }

        @Override
        public String toString() {
            return "(" + value + ", " + count + " ," + leftZeros + " ," + zeros + " ," + rightZeros + ")";
        }
    }
}
