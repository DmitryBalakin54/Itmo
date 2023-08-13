import java.util.Arrays;
import java.util.Scanner;

public class A {
    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);
        int n = scan.nextInt();
        int[][] first = new int[n][n];
        int[][] second = new int[n][n];
        readArray(n, first, scan);
        readArray(n, second, scan);
        scan.close();

        printStats(first);
        printStats(second);
        printComp(first, second);
    }

    public static void printStats(int[][] array) {
        System.out.println(isReflex(array) + " " +
            isAntiReflex(array) + " " +
            isSimmetr(array) + " " +
            isAntiSimmetr(array) + " " +
            isTranz(array));
    }

    public static void printComp(int[][] array1, int[][] array2) {
        for (int i = 0; i < array1.length; i++) {
            for (int j = 0; j < array1.length; j++) {
                int in = 0;
                for (int k = 0; k < array1.length; k++) {
                   if (array1[i][k] == 1 && array2[k][j] == 1) {
                       in = 1;
                       break;
                   }
                }
                System.out.print(in + " ");
            }
            System.out.println();
        }
    }

    public static int isTranz(int[][] array) {
        for (int i = 0; i < array.length; i++) {
            for (int j = 0; j < array.length; j++) {
                for (int k = 0; k < array.length; k++) {
                    if (array[j][i] == 1 && array[i][k] == 1) {
                        if (array[j][k] == 0) {
                            return 0;
                        }
                    }
                }
            }
        }
        return 1;
    }

    public static int isAntiSimmetr(int[][] array) {
        for (int i = 1; i < array.length; i++) {
            for (int j = 0; j < i; j++) {
                if (array[i][j] == 1 && array[j][i] == 1) {
                    return 0;
                }
            }
        }
        return 1;
    }

    public static int isSimmetr(int[][] array) {
        for (int i = 1; i < array.length; i++) {
            for (int j = 0; j < i; j++) {
                if (!((array[i][j] == 1) == (array[j][i] == 1)))  {
                    return 0;
                }
            }
        }
        return 1;
    }

    public static int isReflex(int[][] array) {
        for (int i = 0; i < array.length; i++) {
            if (array[i][i] == 0) {
                return 0;
            }
        }
        return 1;
    }

    public static int isAntiReflex(int[][] array) {
        for (int i = 0; i < array.length; i++) {
            if (array[i][i] == 1) {
                return 0;
            }
        }
        return 1;
    }

    public static void readArray(int n, int[][] array, Scanner scan) {
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                array[i][j] = scan.nextInt();
            }
        }
    }

}
