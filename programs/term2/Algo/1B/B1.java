import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Arrays;

public class B1 {
    public static void main(String[] args) throws IOException {
        mainProgram();
    }
    static void mainProgram() throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        int n = Integer.parseInt(reader.readLine());
        int[] array = new int[n];
        String[] s = reader.readLine().split(" ");
        for (int i = 0; i < n; i++) {
            array[i] = Integer.parseInt(s[i]);
        }
        MaxTree tree = new MaxTree(array);
        //System.err.println(Arrays.toString(tree.array));
        int k = Integer.parseInt(reader.readLine());
        for (int i = 0; i < k; i++) {
            s = reader.readLine().split(" ");
            int left = Integer.parseInt(s[0]);
            int right = Integer.parseInt(s[1]);
            System.out.println(tree.getMax(left, right));
        }
        reader.close();
    }

    static class MaxTree {

        final Pair[] array;

        MaxTree(int[] array) {
            int size = 1;
            while (size < array.length) {
                size <<= 1;
            }
            this.array = new Pair[2 * size - 1];
            for (int i = size - 1; i < size - 1 + array.length; i++) {
                this.array[i] = new Pair(array[i - size + 1], 1);
            }
            build();
        }

        private void build() {
            for (int i = array.length / 2 - 1; i >= 0; i--) {
                array[i] = build(i);
            }
        }

        private Pair build(int index) {
            return  Pair.maxCount(array[2 * index + 1], array[2 * index + 2]);
        }

        public Pair getMax(int qLeft, int qRight) {
            return getMax(0, array.length / 2, array.length - 1, qLeft - 1 + array.length / 2, qRight - 1 + array.length / 2);
        }

        private Pair getMax(int index, int left, int right, int qLeft, int qRight) {
            if (right < qLeft || qRight < left) {
                return null;
            }

            if (qLeft <= left && right <= qRight) {
                return array[index];
            }
            int mid = (left + right) / 2;
            Pair leftPair = getMax(2 * index + 1, left, mid, qLeft, qRight);
            Pair rightPair = getMax(2 * index + 2, mid + 1, right, qLeft, qRight);
            return Pair.maxCount(leftPair, rightPair);
        }
    }

    static class Pair {
        int value;
        final int count;

        public Pair(int value, int index) {
            this.value = value;
            this.count = index;
        }

        static Pair copyOf(Pair p) {
            return new Pair(p.value, p.count);
        }

        static Pair max(Pair p1, Pair p2) {
            if (p1 == null && p2 == null) {
                return null;
            } else if (p1 == null) {
                return p2;
            } else if (p2 == null) {
                return p1;
            }


            if (p1.value > p2.value) {
                return p1;
            } else {
                return p2;
            }
        }

        static Pair maxCount(Pair p1, Pair p2) {
            if (p1 == null && p2 == null) {
                return null;
            } else if (p1 == null) {
                return p2;
            } else if (p2 == null) {
                return p1;
            }


            if (p1.value > p2.value) {
                return p1;
            } else if (p1.value < p2.value) {
                return p2;
            } else {
                return new Pair(p1.value, p1.count + p2.count);
            }
        }

        @Override
        public String toString() {
            return value + " " + count;
        }
    }
}
