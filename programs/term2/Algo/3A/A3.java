import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class A3 {

    static int[] log2;

    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));

        String[] s = reader.readLine().split(" ");
        int n = Integer.parseInt(s[0]);
        int m = Integer.parseInt(s[1]);
        int a1 = Integer.parseInt(s[2]);

        s = reader.readLine().split(" ");
        int u = Integer.parseInt(s[0]);
        int v = Integer.parseInt(s[1]);

        reader.close();

        log2 = new int[n + 1];

        log2[1] = 0;
        for (int i = 2; i <= n; i++) {
            log2[i] = log2[i / 2] + 1;
        }

        int[][] table = new int[n][log2[n] + 1];

        table[0][0] = a1;
        for (int i = 1; i < n; i++) {
            table[i][0] = (23 * table[i - 1][0] + 21563) % 16714589;
        }

        for (int i = n - 1; i >= 0; i--) {
            for (int j = 1; j < log2[n] + 1; j++) {
                table[i][j] = Math.min(table[i][j - 1], table[Math.min(i + (1 << (j - 1)), n - 1)][j - 1]);
            }
        }

        int res = min(u - 1, v - 1, table);
        for (int i = 1; i < m; i++) {
            u = ((17 * u + 751 + res + 2 * i) % n) + 1;
            v = ((13 * v + 593 + res + 5 * i) % n) + 1;
            res = min(u - 1, v - 1, table);
        }

        System.out.println(u + " " + v + " " + res);
    }


    static int min(int left, int right, int[][] table) {
        if (left > right) {
            int tmp = left;
            left = right;
            right = tmp;
        }

        int log = log2[right - left + 1];

        return Math.min(table[left][log], table[right - (1 << log) + 1][log]);
    }

}
