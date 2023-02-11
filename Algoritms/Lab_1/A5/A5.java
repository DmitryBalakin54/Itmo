import java.util.*;

public class A5 {
    public static void main(String[] args) {
        mainProgram();
    }

    public static void mainProgram() {
        Scanner scan = new Scanner(System.in);
        int n = Integer.parseInt(scan.nextLine());
        StringBuilder way = new StringBuilder(scan.nextLine());
        scan.close();

        int[] res = new int[n];
        for (int i = 1; i < n; i++) {
            int max = res[i - 1];
            if (i - 3 >= 0) {
                max = Math.max(max, res[i - 3]);
            }
            if (i - 5 >= 0) {
                max = Math.max(max, res[i - 5]);
            }

            int value = getValue(way.charAt(i));
            if (max == -1 || value == -1) {
                res[i] = -1;
            } else {
                res[i] = max + value;
            }
        }
        System.out.println(res[n - 1]);
    }

    public static int getValue(char ch) {
        if (ch == 'w') {
            return -1;
        } else if (ch == '.') {
            return 0;
        } else {
            return 1;
        }
    }
}
