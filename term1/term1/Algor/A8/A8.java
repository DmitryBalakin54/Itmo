import java.util.Scanner;

public class A8 {
    public static void main(String[] args) {
        mainProgram();
    }

    public static void mainProgram() {
        Scanner scan = new Scanner(System.in);
        int n = scan.nextInt();
        int m = scan.nextInt();
        int[] values = new int[n];
        int sum = 0;
        for (int i = 0; i < n; i++) {
            values[i] = scan.nextInt();
            sum += values[i];
        }

        boolean[][] dp = new boolean[n + 1][sum + 1];
        for (int i = 0; i < n; i++) {
            dp[i + 1][values[i]] = true;
            for (int j = 0; j <= sum; j++) {
                if (dp[i][j]) {
                    dp[i + 1][j] = true;
                    dp[i + 1][j + values[i]] = true;
                }
            }
        }

        for (int i = sum; i >= 0; i--) {
            if (dp[n][i] && i <= m) {
                System.out.println(i);
                break;
            }
        }
    }
}
