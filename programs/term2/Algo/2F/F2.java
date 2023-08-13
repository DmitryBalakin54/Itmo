import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Scanner;

public class F2 {


    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        int n = Integer.parseInt(reader.readLine());

        Window[] beginEdges = new Window[n];
        Window[] endEdges = new Window[n];
        int maxX = 0;
        int minX = 0;
        int maxY = 0;
        int minY = 0;

        for (int i = 0; i < n; i++) {
            String[] nums = reader.readLine().split(" ");
            int lX = Integer.parseInt(nums[0]);
            int lY = Integer.parseInt(nums[1]);
            int rX = Integer.parseInt(nums[2]);
            int rY = Integer.parseInt(nums[3]);

            if (i == 0) {
                maxX = rX;
                maxY = rY;
                minX = lX;
                minY = lY;
            } else {
                maxX = Math.max(maxX, rX);
                maxY = Math.max(maxY, rY);
                minX = Math.min(minX, lX);
                minY = Math.min(minY, lY);
            }

            Window window = new Window(lX, rY, rX, lY);

            beginEdges[i] = window;
            endEdges[i] = window;
        }

        Arrays.sort(beginEdges, Comparator.comparingInt(Window::getLeftX));
        Arrays.sort(endEdges, Comparator.comparingInt(Window::getRightX));

        reader.close();

        int shift = -minY + 1;

        int len = Math.abs(maxY - minY);
        MaxTree tree = new MaxTree(new int[len + 1]);

        int i = 0;
        int j = 0;

        int ansX = 0;
        int ansY = 0;
        int ansMaxWindowsCount = 0;

        while (i < n) {
            if (beginEdges[i].getLeftX() <= endEdges[j].getRightX()) {
                tree.add(beginEdges[i].getLeftY() + shift, beginEdges[i].getRightY() + shift, 1);
                int currentWindowsCount = tree.getMax(1, len);
                if (currentWindowsCount > ansMaxWindowsCount) {
                    ansX = beginEdges[i].getLeftX();
                    ansY = tree.getMaxIndex(currentWindowsCount) - shift;
                    ansMaxWindowsCount = currentWindowsCount;
                }
                i++;
                continue;
            }

            tree.add(endEdges[j].getLeftY() + shift, endEdges[j].getRightY() + shift, -1);
            j++;
        }

        System.out.println(ansMaxWindowsCount);
        System.out.println(ansX + " " + ansY);
    }

    static class Dot {
        final int x;
        final int y;


        public Dot(int x, int y) {
            this.x = x;
            this.y = y;
        }

        static Dot copyDot(Dot dot) {
            return new Dot(dot.x, dot.y);
        }
    }

    static class Window {

        final Dot leftTop;
        final Dot rightBottom;

        public Window(Dot leftTop, Dot rightBottom) {
            this.leftTop = leftTop;
            this.rightBottom = rightBottom;
        }

        public Window(int lX, int lY, int rX, int rY) {
            leftTop = new Dot(lX, lY);
            rightBottom = new Dot(rX, rY);
        }

        public int getLeftX() {
            return leftTop.x;
        }

        public int getLeftY() {
            return rightBottom.y;
        }

        public int getRightX() {
            return rightBottom.x;
        }

        public int getRightY() {
            return leftTop.y;
        }



        public Dot getLeftTop() {
            return Dot.copyDot(leftTop);
        }

        public Dot getRightBottom() {
            return Dot.copyDot(rightBottom);
        }

        public Dot getLeftBottom() {
            return new Dot(leftTop.x, rightBottom.y);
        }

        public Dot getRightTop() {
            return new Dot(rightBottom.x, leftTop.y);
        }
    }


    static class MaxTree {

        final int[] array;
        final int[] add;


        MaxTree(int[] array) {
            int size = 1;
            while (size < array.length) {
                size <<= 1;
            }
            this.array = new int[2 * size - 1];
            this.add = new int[2 * size - 1];
            for (int i = size - 1; i < size - 1 + array.length; i++) {
                this.array[i] = array[i - size + 1];
            }
            for (int i = size - 1 + array.length; i < this.array.length; i++) {
                this.array[i] = Integer.MIN_VALUE;
            }
            build();
        }

        private void build() {
            for (int i = array.length / 2 - 1; i >= 0; i--) {
                array[i] = build(i);
            }
        }

        private int build(int index) {
            return array[index] = Math.max(array[2 * index + 1], array[2 * index + 2]);
        }

        public int getMax(int qLeft, int qRight) {
            return getMax(0, array.length / 2, array.length - 1, qLeft - 1 + array.length / 2, qRight - 1 + array.length / 2);
        }

        private int getMax(int index, int left, int right, int qLeft, int qRight) {
            if (right < qLeft || qRight < left) {
                return Integer.MIN_VALUE;
            }

            if (qLeft <= left && right <= qRight) {
                //return Pair.copyOf(array[index]);
                return array[index];
            }
            push(index);
            int mid = (left + right) / 2;
            int leftSon = getMax(2 * index + 1, left, mid, qLeft, qRight);
            int rightSon = getMax(2 * index + 2, mid + 1, right, qLeft, qRight);
            return Math.max(leftSon, rightSon);

        }

        public int getMaxIndex(int max) {
            int index = 0;
            while (index < array.length / 2) {
                push(index);
                
                if (array[2 * index + 1] == max) {
                    index = 2 * index + 1;
                } else {
                    index = 2 * index + 2;
                }
            }

            return index - array.length / 2 + 1;
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
                return p2;
            }
        }

        @Override
        public String toString() {
            return "(" + value + ", " + index + " ," + add + ")";
        }
    }
}
