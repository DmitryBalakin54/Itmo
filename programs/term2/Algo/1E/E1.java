import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.Scanner;

public class E1 {
    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));

        String[] s = reader.readLine().split(" ");
        int n = Integer.parseInt(s[0]);
        int m = Integer.parseInt(s[1]);

        int[] array = new int[n];

        s = reader.readLine().split(" ");
        for (int i = 0; i < n; i++) {
            array[i] = Integer.parseInt(s[i]);

        }

        MaxTree tree = new MaxTree(array);
        for (int i = 0; i < m; i++) {
            s = reader.readLine().split(" ");
            int operation = Integer.parseInt(s[0]);
            int index = Integer.parseInt(s[1]);
            int value = Integer.parseInt(s[2]);

            if (operation == 0) {
                tree.update(index, value);
            } else {
                System.out.println(tree.get(index, value));
            }

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
                this.array[i] = new Pair(array[i - size + 1], i - size + 2);
            }
            build();
        }

        private void build() {
            for (int i = array.length / 2 - 1; i >= 0; i--) {
                array[i] = build(i);
            }
        }

        private Pair build(int index) {
            return  Pair.max(array[2 * index + 1], array[2 * index + 2]);
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
            return Pair.max(leftPair, rightPair);
        }

        public void update(int index, int value) {
            index = array.length / 2 - 1 + index;

            array[index].value = value;

            index = (index - 1) / 2;

            while (true) {
                array[index] = Pair.copyOf(Pair.max(array[2 * index + 1], array[2 * index + 2]));

                if (index == 0) {
                    break;
                }
                index = (index - 1) / 2;
            }
        }

        public int get(int ind, int value) {
            Pair res = get(0, array.length / 2, array.length - 1, array.length / 2 + ind - 1, array.length - 1, value);

            return res == null ? -1 : res.index;
        }

        private Pair get(int index, int left, int right, int qLeft, int qRight, int value) {
            if (right < qLeft || qRight < left) {
                return null;
            }

            int mid = (left + right) / 2;
            Pair leftPair;
            Pair rightPair;

            if (qLeft <= left && right <= qRight) {
                if (qLeft <= index) {
                    return Pair.maxI(array[index], null, value);
                }

                leftPair = getMax(2 * index + 1, left, mid, qLeft, qRight);
                if (leftPair != null) {
                    if (leftPair.value >= value) {
                        return get(2 * index + 1, left, mid, qLeft, qRight, value);
                    }
                }

                rightPair = getMax(2 * index + 2, mid + 1, right,  qLeft, qRight);
                if (rightPair != null) {
                    if (rightPair.value >= value) {
                        return get(2 * index + 2, mid + 1, right,   qLeft, qRight, value);
                    }
                }

                return null;
            }

            leftPair = get(2 * index + 1, left, mid, qLeft, qRight, value);
            rightPair = get(2 * index + 2, mid + 1, right, qLeft, qRight, value);
            return Pair.maxI(leftPair, rightPair, value);
        }



    }



    static class Pair {
        int value;
        final int index;

        public Pair(int value, int index) {
            this.value = value;
            this.index = index;
        }

        static Pair copyOf(Pair p) {
            if (p == null) {
                return null;
            }
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

        static Pair maxI(Pair p1, Pair p2, int value) {
            if (p1 == null && p2 == null) {
                return null;
            } else if (p1 == null) {
                //return copyOf(p2);
                return p2.value >= value ? p2 : null;
            } else if (p2 == null) {
                //return copyOf(p1);
                return p1.value >= value ? p1 : null;
            }


            if (p1.index > p2.index) {
                Pair tmp = p1;
                p1 = p2;
                p2 = tmp;
            }

            if (p1.value >= value) {
                //return copyOf(p1);
                return p1;
            } else if (p2.value >= value) {
                return p2;
            } else {
                return null;
            }
        }

        @Override
        public String toString() {
            return value + " " + index;
        }
    }
}
