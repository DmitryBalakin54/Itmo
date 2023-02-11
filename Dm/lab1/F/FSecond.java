import java.util.*;

public class FSecond {

    static boolean anti = false;

    public static void main(String[] args) {
        mainProgram();
    }

    public  static  void mainProgram() {
        Scanner scan = new Scanner(System.in);
        int n = scan.nextInt();
        int k = scan.nextInt();
        boolean[] is = new boolean[n];
        boolean[] not = new boolean[n];
        int[][] array = new int[k][n];
        for (int i = 0; i < k; i++) {
            for (int j = 0; j < n; j++) {
                array[i][j] = scan.nextInt();
            }
        }
        scan.close();
        while (searchOne(k, n, is, not, array)) {
            for (int i = 0; i < n; i++) {
                if (is[i] && not[i]) {
                    System.out.println("YES");
                    return;
                }
            }
        }
        if (anti) {
            System.out.println("YES");
            return;
        }
        System.out.println("NO");
    }

    public static boolean searchOne(int k, int n, boolean[] is, boolean[] not, int[][] array) {
        int counter = 0;
        List<Integer> list = new ArrayList<>();
        line : for (int i = 0; i < k; i++) {
            int countPos = 0;
            int countNeg = 0;
            int flagPos = -1;
            int flagNeg = -1;
            int antiCounter = 0;
            for (int j = 0; j < n; j++) {
                if (is[j] && array[i][j] == 1) {
                    continue line;
                }
                antiCounter += (array[i][j] == -1) ? 1 : 0;
                if (array[i][j] == 1) {
                    flagPos = j;
                    countPos++;
                } else if (array[i][j] == 0) {
                    flagNeg = j;
                    countNeg++;
                }
            }
            if (antiCounter == n) {
                anti = true;
            }
            if (countPos == 1 && countNeg == 0) {
                is[flagPos] = true;
                list.add(flagPos);
                counter++;
            } else if (countNeg == 1 && countPos == 0) {
                not[flagNeg] = true;
            }
        }
        if (counter > 0) {
            for (Integer integer : list) {
                for (int j = 0; j < k; j++) {
                    array[j][integer] = (array[j][integer] == 0) ? -1 : array[j][integer];
                }
            }
            return true;
        }
        return false;
    }
}
