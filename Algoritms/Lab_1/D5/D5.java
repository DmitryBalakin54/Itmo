import java.util.Arrays;
import java.util.Scanner;

public class D5 {
    public static void main(String[] args) {
        mainProgram();
    }

    public static void mainProgram() {
        Scanner scan = new Scanner(System.in);
        int n = scan.nextInt();
        int[] firstSequence = new int[n + 1];
        for (int i = 1; i <= n; i++) {
            firstSequence[i] = scan.nextInt();
        }
        int m = scan.nextInt();
        int[] secondSequence = new int[m + 1];
        for (int i = 1; i <= m; i++) {
            secondSequence[i] = scan.nextInt();
        }
        scan.close();

        int[][] dp = new int[n + 1][m + 1];
        for (int i = 1; i <=n; i++) {
            for (int j = 1; j <= m; j++) {
                if (firstSequence[i] == secondSequence[j]) {
                    dp[i][j] = dp[i - 1][j - 1] + 1;
                } else {
                    dp[i][j] = Math.max(dp[i - 1][j], dp[i][j - 1]);
                }
            }
        }
        System.out.println(dp[n][m]);
//        for (int i = 0; i <= n; i++) {
//            System.err.println(Arrays.toString(dp[i]));
//        }
        int column = m;
        int string = n;
        int counter = dp[n][m];
        StringBuilder answer = new StringBuilder();
        while (counter > 0) {
            if (column > 0) {
                if (dp[string][column - 1] == dp[string][column]) {
                    column--;
                    continue;
                }
            }
            if (string > 0) {
                if (dp[string - 1][column] == dp[string][column]) {
                    string--;
                    continue;
                }
            }

            if (dp[string - 1][column - 1] + 1 == dp[string][column]) {
                counter--;
                answer.insert(0, firstSequence[string] + " ");
                column--;
                string--;
            }
        }
        System.out.println(answer);
    }
}
