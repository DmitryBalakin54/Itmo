import java.util.*;

public class task15 {
    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);
        int n = scan.nextInt();
        int k = scan.nextInt();
        int m = scan.nextInt();
        scan.close();

        StringBuilder result = new StringBuilder();
        int lastNum = 0;
        for (int i = 0; i < k; i++) {
            long j = -1;
            for (int l = 0; l < n; l++) {
                long c = c(n - lastNum - 1 - l, k - i - 1);
               // System.err.println(c);
                j += c;
                if (j >= m) {
                    lastNum += l + 1;
                    m -= j - c + 1;
                    break;
                }
            }
            result.append(lastNum).append(" ");
        }
        System.out.println(result);
    }

    public static long fact(long n) {
        if (n == 0) {
            return 1;
        }

        return n * fact(n - 1);
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

//    public static int searchNum(int n, int k, int m, int lastNum) {
//        long j = 0;
//        for (int i = 0; i < m; i++) {
//            j += c(n - i - 1, k);
//            if (j > m) {
//                j -= c(n - i, k);
//                return
//            }
//        }
//    }
}
