import java.io.*;
import java.util.*;

public class E {
    public static void main(String[] args) throws IOException {
        mainProgram();
    }

    public static void mainProgram() throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        int n = Integer.parseInt(reader.readLine());
        int k = (int) Math.pow(2, n);
        boolean[][] arr = new boolean[k][n + 1];
        for (int i = 0; i < k; i++) {
            StringBuilder str = new StringBuilder(reader.readLine());
            for (int j = 0; j < n; j++) {
                arr[i][j] = (str.charAt(j) == '1');
            }
            arr[i][n] = (str.charAt(n + 1) == '1');
        }
        reader.close();
        makeArray(arr, k, n + 1);
        for (int i = 0; i < k; i++) {
            for (int j = 0; j < n; j++) {
                System.out.print( ((arr[i][j]) ? 1 : 0));
            }
            System.out.println(" " + ((arr[i][n]) ? 1 : 0));
        }
    }

    public static void makeArray(boolean arr[][], int size, int length) {
        for (int i = 1; i < size; i++) {
            boolean result = arr[0][length - 1];
            for (int j = 1; j < i; j++) {
                boolean is = true;
                for (int l = 0; l < length - 1; l++) {
                    if (arr[j][l] && !arr[i][l]) {
                        is = false;
                        break;
                    }
                }
                if (is) {
                    result ^= arr[j][length - 1];
                }
            }
            arr[i][length - 1] = (result) ? !arr[i][length - 1] : arr[i][length - 1];
        }
    }
}
