import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class D1 {


    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        int n = Integer.parseInt(reader.readLine());

        int[] array = new int[n];
        String[] nums = reader.readLine().split(" ");
        for (int i = 0; i < n; i++) {
            array[i] = Integer.parseInt(nums[i]);
        }

        ZerosCountTree tree = new ZerosCountTree(array);

        int m = Integer.parseInt(reader.readLine());
        for (int i = 0; i < m; i++) {

            nums = reader.readLine().split(" ");
            char ch = nums[0].charAt(0);
            if (ch == 's') {
                System.out.print(tree.getIndex(Integer.parseInt(nums[1]), Integer.parseInt(nums[2]), Integer.parseInt(nums[3])) + " ");
            } else if (ch == 'u') {
                tree.update(Integer.parseInt(nums[1]), Integer.parseInt(nums[2]));
            }
            //System.err.println(tree);
        }

        reader.close();
    }





    static class ZerosCountTree {

        final int[] array;
        final int[] leftZeros;
        final int[] rightZeros;

        final int length;

        ZerosCountTree(int[] array) {
            int size = 1;
            while (size < array.length) {
                size <<= 1;
            }

            this.array = new int[2 * size - 1];
            this.leftZeros = new int[2 * size - 1];
            this.rightZeros = new int[2 * size - 1];
            this.length = array.length;

            for (int i = size - 1; i < size - 1 + array.length; i++) {
                this.array[i] = array[i - size + 1];
                if (this.array[i] == 0) {
                    leftZeros[i] = 1;
                } else {
                    leftZeros[i] = 0;
                }
                rightZeros[i] = 0;
            }
            build();
        }

        private int getZeros(int index) {
            return leftZeros[index] + rightZeros[index];
        }

        private int getZerosOnSegment(int index, int left, int right, int qLeft, int qRight) {
            if (right < qLeft || qRight < left) {
                return 0;
            }

            if (qLeft <= left && right <= qRight) {
                return getZeros(index);
            }

            int mid = (left + right) / 2;
            int leftSon = getZerosOnSegment(2 * index + 1, left, mid, qLeft, qRight);
            int rightSon = getZerosOnSegment(2 * index + 2, mid + 1, right, qLeft, qRight);
            return leftSon + rightSon;
        }
        private void build() {
            for (int i = array.length / 2 - 1; i >= 0; i--) {
                build(i);
            }
        }

        private void build(int index) {
            array[index] = 0;
            leftZeros[index] = getZeros(2 * index + 1);
            rightZeros[index] = getZeros(2 * index + 2);
        }



        public int getIndex(int qLeft, int qRight, int k) {
            return getIndex(0, array.length / 2, array.length - 1, qLeft - 1 + array.length / 2, qRight - 1 + array.length / 2, k);
        }

        private int getIndex(int index, int left, int right, int qLeft, int qRight, int k) {
            if (right < qLeft || qRight < left || k <= 0) {
                return -1;
            }

            if (index > array.length / 2 + length - 1) {
                return  -1;
            }

            if (array.length / 2 <= index && index <= array.length / 2 + length - 1) {
                if (k == 1 && array[index] == 0) {
                    return index - array.length / 2 + 1;
                } else {
                    return -1;
                }
            }


            int mid = (left + right) / 2;
            int leftZeros = getZerosOnSegment(2 * index + 1, left, mid, qLeft, qRight);
            int rightZeros = getZerosOnSegment(2 * index + 2, mid + 1, right, qLeft, qRight);

            if (leftZeros + rightZeros < k) {
                return -1;
            }

            int leftSon = getIndex(2 * index + 1, left, mid, qLeft, qRight, k);
            int rightSon = getIndex(2 * index + 2, mid + 1, right, qLeft, qRight, k - leftZeros);
            return Math.max(leftSon, rightSon);

        }



        public void update(int index, int val) {
            index = array.length / 2 + index - 1;

            array[index] = val;
            leftZeros[index] = val == 0 ? 1 : 0;

            if (index == 0) {
                return;
            }

            index = (index - 1) / 2;

            while (true) {

                leftZeros[index] = getZeros(2 * index + 1);
                rightZeros[index] = getZeros(2 * index + 2);

                if (index == 0) {
                    break;
                }

                index = (index - 1) / 2;
            }

        }

        @Override
        public String toString() {
            StringBuilder res = new StringBuilder();

            int k = 1;
            int two = 1;
            for (int i = 0; i < array.length; i++) {
                res.append("(" + array[i] + ", " + leftZeros[i] + ", " + rightZeros[i] + ")");

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

}
