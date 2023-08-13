import java.util.*;

public class C5 {
    static final int MAX_VALUE = 1000000001;

    public static void main(String[] args) {
        mainProgram();
    }

    public static void mainProgram() {
        Scanner scan = new Scanner(System.in);
        int n = scan.nextInt();
        int[] sequence = new int[n];
        for (int i = 0; i < n; i++) {
            sequence[i] = scan.nextInt();
        }
        scan.close();

        int[] dp = new int[n];
        int[] last = new int[n];
        for (int i = 0; i < n; i++) {
            dp[i] = 1;
            last[i] = -1;
            for (int j = 0; j < i; j++) {
                if (sequence[j] < sequence[i] && dp[j] + 1 > dp[i]) {
                    dp[i] = dp[j] + 1;
                    last[i] = j;
                }
            }
        }

        int index = 0;
        int length = dp[0];
        for (int i = 0; i < n; i++) {
            if (dp[i] > length) {
                index = i;
                length = dp[i];
            }
        }

        StringBuilder res = new StringBuilder();
        while (index != -1) {
            res.insert(0, sequence[index] + " ");
            index = last[index];
        }
        System.out.println(length);
        System.out.println(res);

    }
}
