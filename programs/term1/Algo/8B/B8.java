import java.util.Scanner;

public class B8 {
    public static void main(String[] args) {
        mainProgram();
    }

    public static void mainProgram() {
        Scanner scan = new Scanner(System.in);
        int n = scan.nextInt();
        int m = scan.nextInt();

        int[] weight = new int[n + 1];
        int[] cost = new int[n + 1];
        int sum = 0;
        for (int i = 1; i < n + 1; i++) {
            weight[i] = scan.nextInt();
            sum += weight[i];
        }
        for (int i = 1; i < n + 1; i++) {
            cost[i] = scan.nextInt();
        }
        scan.close();

        int[][] dp = new int[n + 1][sum + 1];

        for (int i = 1; i < n + 1; i++) {
            dp[i][weight[i]] = Math.max(cost[i], dp[i - 1][weight[i]]);
            for (int j = 1; j < sum + 1; j++) {
                int value = dp[i - 1][j];
                if (j - weight[i] >= 0 && dp[i - 1][j - weight[i]] > 0) {
                    value = Math.max(value, dp[i - 1][j - weight[i]] + cost[i]);
                }
                dp[i][j] = Math.max(value, dp[i][j]);
            }
        }

//        for (int i = 0; i <= n; i++) {
//            for (int j = 0; j <= sum; j++) {
//                System.out.print(dp[i][j] + " ");
//            }
//            System.out.println();
//        }

        int res = 0;
        int index = 0;
        for (int i = m; i >= 0; i--) {
            if (i <= sum) {
                if (res < dp[n][i]) {
                    res = dp[n][i];
                    index = i;
                }
            }
        }
       // System.out.println(res + " " + index);
        int counter = 0;
        StringBuilder str = new StringBuilder();
        for (int i = n; i > 0; i--) {
            if (dp[i][index] != dp[i - 1][index]) {
                if (index - weight[i] >= 0) {
                    index = index - weight[i];
                    counter++;
                    str.insert(0, i + " ");
                } else {
                    break;
                }
            }
        }
        System.out.println(counter);
        System.out.println(str);
    }
}
