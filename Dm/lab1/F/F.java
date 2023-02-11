import java.util.*;

public class F {
    public static void main(String[] args) {
        mainProgram();
    }

    public static void mainProgram() {
        Scanner scan = new Scanner(System.in);
        int n = scan.nextInt();
        int k = scan.nextInt();
        int amount = 0;
        int amount2 = 0;
        boolean anti = false;
        int[] countOne = new int[n];
        for (int i  = 0; i < k; i++) {
            int flag = 0;
            int count = 0;
            int countAnti = 0;
            int count2 = 0;
            for (int j = 0; j < n; j++) {
                int g = scan.nextInt();
                if (g == 1) {
                    count += 1;
                    flag = j;
                }
                if (g == 0) {
                    countAnti++;
                    count2++;
                }
            }
            amount2 += (count2 == 1 && count == 0) ? 1 : 0;
            if (count == 1 && count2 == 0) {
                amount += 1;
                countOne[flag] = 1;
            }
            anti = (count == 0 && count2 > 0) ? true : anti;
        }
        scan.close();
        boolean is;
        int c = 0;
        for (int i = 0; i < n; i++) {
            c += countOne[i];
        }
        is = (c == n);
        System.out.println( ((is && anti) || (is && amount2 > 0) ) ? "YES" : "NO");
    }

}
