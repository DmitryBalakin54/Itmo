import java.util.*;

public class task16 {
    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);
        int n = scan.nextInt();
        int k = scan.nextInt();
        int[] vector = new int[k];
        for (int i = 0; i < k; i++) {
            vector[i] = scan.nextInt();
        }
        scan.close();
        long result = 0;
        int lastNum = 0;
        for (int i = 0; i < k; i++) {
            for (int j = lastNum + 1; j < vector[i]; j++) {
                result += c(n - j, k - i - 1);
                //System.err.println(result + " " + vector[i]);
            }
            lastNum = vector[i];
        }
        System.out.println(result);
    }

    public static long c(long n, long k) {
        long result = 1;
        if (k > n - k) {
            for (int i = (int) (k + 1); i <= n; i++) {
                result *= i;
            }
            for (int i = 1; i <= n - k; i++) {
                result /= i;
            }
        } else {
            for (int i = (int) (n - k + 1); i <= n; i++) {
                result *= i;
            }
            for (int i = 1; i <= k; i++) {
                result /= i;
            }
        }
        return result;
    }
}
