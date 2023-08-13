import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class B3 {

    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        int n = Integer.parseInt(reader.readLine());

        StarsTable table = new StarsTable(n);

        while (true) {
            String[] s = reader.readLine().split(" ");

            int op = Integer.parseInt(s[0]);

            if (op == 1) {
                table.add(
                        Integer.parseInt(s[1]),
                        Integer.parseInt(s[2]),
                        Integer.parseInt(s[3]),
                        Integer.parseInt(s[4]));

            } else if (op == 2) {
                System.out.println(table.getSum(
                        Integer.parseInt(s[1]),
                        Integer.parseInt(s[2]),
                        Integer.parseInt(s[3]),
                        Integer.parseInt(s[4]),
                        Integer.parseInt(s[5]),
                        Integer.parseInt(s[6])));

            } else if (op == 3) {
                break;
            }
        }
        reader.close();
    }

    static class StarsTable {

        final int[][][] table;
        final int n;

        public StarsTable(int n) {
            this.n = n;
            this.table = new int[n + 1][n + 1][n + 1];
        }

        public int getSum(int x1, int y1, int z1, int x2, int y2, int z2) {
            return getSum(x2 + 1, y2 + 1, z2 + 1) -
                    getSum(x1, y1, z1) -
                    getSum(x2 + 1, y2 + 1, z1) -
                    getSum(x2 + 1, y1, z2 + 1) -
                    getSum(x1, y2 + 1, z2 + 1) +
                    getSum(x1, y1, z2 + 1) +
                    getSum(x1, y2 + 1, z1) +
                    getSum(x2 + 1, y1, z1);
        }

        private int getSum(int x, int y, int z) {
            int res = 0;
            int x1 = x;

            while (x1 > 0) {
                int y1 = y;

                while (y1 > 0) {
                    int z1 = z;

                    while (z1 > 0) {
                        res += table[x1][y1][z1];
                        z1 -= z1 & -z1;
                    }

                    y1 -= y1 & -y1;
                }

                x1 -= x1 & -x1;
            }
            return res;
        }

        public void add(int x, int y, int z, int value) {
            int x1 = x + 1;

            while (x1 <= n) {
                int y1 = y + 1;

                while (y1 <= n) {
                    int z1 = z + 1;

                    while (z1 <= n) {
                        table[x1][y1][z1] += value;
                        z1 += z1 & -z1;
                    }

                    y1 += y1 & -y1;
                }

                x1 += x1 & -x1;
            }
        }
    }
}
