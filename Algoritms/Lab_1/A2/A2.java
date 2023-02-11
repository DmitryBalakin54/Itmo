import java.util.*;


public class A2 {
    public static void main(String[] args) {
        mainProgram();
    }

    public static void mainProgram() {
        Scanner scan = new Scanner(System.in);
        int n = scan.nextInt();
        int m = scan.nextInt();
        int k = scan.nextInt();
        int[][]  vectors = generate(n);
        for (int j = 0; j < k; j++) {
            int r = scan.nextInt();
            for (int i = 0; i < r; i++) {
                swap(vectors, scan.nextInt() - 1, scan.nextInt() - 1);
            }
        }
        if (isSorted(vectors)) {
            System.out.println("YES");
        } else {
            System.out.println("NO");
        }
        scan.close();
    }

    public static boolean isSorted(int[][] array) {
        for (int[] arr : array) {
            for (int i = 1; i < arr.length; i++) {
                if (arr[i] < arr[i - 1]) {
                    return false;
                }
            }
        }
        return true;
    }

    public static void swap(int[][] array, int index1, int index2) {
        if (index1 != index2) {
            for (int[] i : array) {
                if (i[Math.max(index1, index2)] < i[Math.min(index1, index2)] ) {
                    swap(i, index1, index2);
                }
            }
        }
    }

    public static void swap(int[] array, int index1, int index2) {
        if (index1 != index2) {
            int a = array[index1];
            array[index1] = array[index2];
            array[index2] = a;
        }
    }
    public static int[][] generate(int n) {
        int[][] result = new int [1 << n][n];
        for (int i = 0; i < n; i++) {
            result[0][i] = 0;
        }

        for (int j = 1; j < 1 << n; j++) {
            boolean flag = true;
            for (int i =  n - 1; i >= 0; i--) {
                if (result[j - 1][i] == 0 && flag) {
                    result[j][i] = 1;
                    flag = false;
                } else if (result[j - 1][i] == 1 && flag) {
                    result[j][i] = 0;
                } else {
                    result[j][i] = result[j - 1][i];
                }
            }
        }
        return result;
    }
}
