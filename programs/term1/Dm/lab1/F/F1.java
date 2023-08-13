
import java.util.Arrays;
import java.util.Scanner;

public class F1 {
    static short k;
    static short l;
    static short chV = -1;
    static int chVN = 0;

    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);
        l = scan.nextShort();
        k = scan.nextShort();
        int[][] arr = new int[k][l];
        int[] v = new int[k];
        scan.nextLine();
        for (int i = 0; i < k; i++) {
            String line = scan.nextLine();
            Scanner lSc = new Scanner(line);
            for (int j = 0; j < l; j++) {
                arr[i][j] = lSc.nextInt();
            }
        }
        while (haveSin(arr)) {
            for (int i = 0; i < k; i++) {
                if (arr[i][chV] != -1) {
                    int k = arr[i][chV] == chVN  ? 1 : 0;
                    v[i] = k | v[i];
                    arr[i][chV] = -1;
                    if (v[i] == 0) {
                        int counter = 0;
                        for (short j = 0; j < l; j++) {
                            if (arr[i][j] != -1) {
                                counter++;
                            }
                        }
                        if (counter == 0) {
                            System.out.println("YES");
                            return;
                        }
                    }
                    else{
                        for (short j = 0; j < l; j++) {
                            arr[i][j] =-1;
                        }
                    }
                }
            }
        }
        System.out.println("NO");

    }

    private static boolean haveSin(int[][] arr) {
        for (int i = 0; i < k; i++) {
            int counter = 0;
            for (short j = 0; j < l; j++) {
                if (arr[i][j] != -1) {
                    counter++;
                    chV = j;
                    chVN = arr[i][j];
                }
            }
            if (counter == 1) {
                return true;
            }
        }
        return false;
    }

}