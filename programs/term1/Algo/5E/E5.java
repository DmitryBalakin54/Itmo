import java.util.Scanner;

public class E5 {
    public static void main(String[] args) {
        mainProgram();
    }

    public static void mainProgram() {
        Scanner scan = new Scanner(System.in);
        StringBuilder firstString = new StringBuilder(scan.nextLine());
        StringBuilder secondString = new StringBuilder(scan.nextLine());
        scan.close();

        int[][] dp = new int[firstString.length() + 1][secondString.length() + 1];
        for (int i = 0; i <= firstString.length(); i++) {
            for (int j = 0; j <= secondString.length(); j++) {
                if (i == 0 || j == 0) {
                    dp[i][j] = i + j;
                    continue;
                }
                if (firstString.charAt(i - 1) == secondString.charAt(j - 1)) {
                    dp[i][j] = dp[i - 1][j - 1];
                } else {
                    dp[i][j] = Math.min(Math.min(dp[i - 1][j - 1], dp[i - 1][j]), dp[i][j - 1]) + 1;
                }
            }
        }
        System.out.println(dp[firstString.length()][secondString.length()]);
    }
}
