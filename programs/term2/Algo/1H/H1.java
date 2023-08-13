import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class H1 {
    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        char[] array = reader.readLine().toCharArray();

        SequenceTree tree = new SequenceTree(array);

        int k = Integer.parseInt(reader.readLine());
        for (int i = 0; i < k; i++) {
            String[] s = reader.readLine().split(" ");
            int left = Integer.parseInt(s[0]);
            int right = Integer.parseInt(s[1]);
            System.out.println(tree.getMax(left, right));
        }
        reader.close();
    }


    static class SequenceTree {

        final Pair[] array;

        SequenceTree(char[] array) {
            int size = 1;
            while (size < array.length) {
                size <<= 1;
            }
            this.array = new Pair[2 * size - 1];
            for (int i = size - 1; i < size - 1 + array.length; i++) {
                int open = array[i - size + 1] == '(' ? 1 : 0;
                int close = open == 0 ? 1 : 0;
                this.array[i] = new Pair(close, 0, open);
            }
            build();
        }

        private void build() {
            for (int i = array.length / 2 - 1; i >= 0; i--) {
                build(i);
            }
        }

        private void build(int index) {
            array[index] = Pair.sequence(array[2 * index + 1], array[2 * index + 2]);
        }

        public int getMax(int qLeft, int qRight) {
            Pair res = getMax(0, array.length / 2, array.length - 1, qLeft - 1 + array.length / 2, qRight - 1 + array.length / 2);
            if (res == null) {
                return 0;
            }

            return res.sequence;
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
            return Pair.sequence(leftPair, rightPair);
        }
    }

    static class Pair {
        int open;
        int sequence;

        int close;

        public Pair(int close, int sequence, int open) {
            this.open = open;
            this.sequence = sequence;
            this.close = close;
        }

        static Pair copyOf(Pair p) {
            return new Pair(p.close, p.sequence, p.open);
        }

        static Pair sequence(Pair l, Pair r) {
            if (l == null) {
                return r;
            }

            if (r == null) {
                return l;
            }

            int newSeq = Math.min(l.open, r.close);
            return new Pair(l.close + r.close - newSeq, l.sequence + r.sequence + 2 * newSeq, l.open + r.open - newSeq);
        }

        @Override
        public String toString() {
            return open + " " + sequence;
        }
    }
}

