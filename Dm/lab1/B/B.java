import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Arrays;

public class B {
    public static void main(String[] args) throws IOException {
        mainProgram();
    }

    public static void mainProgram() throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        int n = Integer.parseInt(reader.readLine());
        boolean[][] arr = new boolean[n][];
        for (int i = 0; i < n; i++) {
            StringBuilder str = new StringBuilder(reader.readLine());
            int index = str.lastIndexOf(" ") + 1;
            int k = Integer.parseInt(str.substring(0, index - 1));
            arr[i] = new boolean[(int) Math.pow(2, k)];
           // System.err.println(arr[i].length + " " + index + " " + str.length());
            for (int h = 0; h < arr[i].length; h++) {
                arr[i][h] = (str.charAt(h + index) == '1');
            }
        }
        reader.close();
        System.out.println((checkAll(arr)) ? "YES" : "NO");
    }

    public static boolean checkAll(boolean[][] arr) {
        for (int i = 0; i < arr.length; i++) {
            if (checkNotSafeZero(arr[i])) {
               // System.err.println("Z" + Arrays.toString(arr[i]));
                break;
            }
            if (i == arr.length - 1) {
                return false;
            }
        }
        for (int i = 0; i < arr.length; i++) {
            if (checkNotSafeOne(arr[i])) {
               // System.err.println("O" + Arrays.toString(arr[i]));
                break;
            }
            if (i == arr.length - 1) {
                return false;
            }
        }
        for (int i = 0; i < arr.length; i++) {
            if (checkNotSelfDual(arr[i])) {
               // System.err.println("D" + Arrays.toString(arr[i]));
                break;
            }
            if (i == arr.length - 1) {
                return false;
            }
        }
        for (int i = 0; i < arr.length; i++) {
            if (checkNotMonotony(arr[i])) {
                //System.err.println("M" + Arrays.toString(arr[i]));
                break;
            }
            if (i == arr.length - 1) {
                return false;
            }
        }
        for (int i = 0; i < arr.length; i++) {
            if (checkNotLinear(arr[i])) {
                //System.err.println("L" + Arrays.toString(arr[i]));
                break;
            }
            if (i == arr.length - 1) {
                return false;
            }
        }
        return true;
    }

    public static boolean checkNotSafeOne(boolean[] arr) {
        return !arr[arr.length - 1];
    }

    public static boolean checkNotSafeZero(boolean[] arr) {
        return arr[0];
    }

    public static boolean checkNotSelfDual(boolean[] arr) {
        for (int i = 0; i < arr.length; i++) {
            if (arr[i] == arr[arr.length - 1 - i]) {
                return true;
            }
        }
        return false;
    }

    public static boolean checkNotMonotony(boolean[] arr) {
        boolean[][] array = buildArray(arr);
        int size = array.length;
        int length = array[0].length;
        for (int i = 1; i < size; i++) {
            for (int j = 0; j < i; j++) {
                if (isDominate(array, j, i)) {
                    if (!array[i][length - 1] && array[j][length - 1]) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    public static boolean checkNotLinear(boolean[] arr) {
        boolean[][] array = buildArray(arr);
        int size = array.length;
        int length = array[0].length;
        for (int i = 1; i < size; i++) {
            boolean result = array[0][length - 1];
            for (int j = 1; j < i; j++) {
                if (isDominate(array, j, i)) {
                    result ^= array[j][length - 1];
                }
            }
            array[i][length - 1] = (result) ? !array[i][length - 1] : array[i][length - 1];
            if (!(i == 1 || i == 2 || i == 4 || i == 8 || i == 16 || i == 32)) {
                if (array[i][length - 1]) {
                    return true;
                }
            }
        }
        return false;
    }

    public static boolean[][] buildArray(boolean[] arr) {
        boolean[][] array = new boolean[arr.length][
            logTwo(arr.length) + 1];
        for (int i = 0; i < arr.length; i++) {
            array[i][array[i].length - 1] = arr[i];
        }
        for (int i = 1; i < array.length; i++) {
            for (int j = array[i].length - 2; j >=0 ; j--) {
                if (!array[i - 1][j]) {
                    array[i][j] = true;
                    for (int h = j - 1; h >= 0; h--) {
                        array[i][h] = array[i - 1][h];
                    }
                    break;
                } else {
                    array[i][j] = false;
                }
            }
        }
//        for (int i = 0; i <array.length; i++) {
//            System.err.println(Arrays.toString(array[i]));
//        }
        return array;
    }

    public static int logTwo(int num) {
        int count = 0;
        while (num % 2 == 0) {
            num /=2;
            count++;
        }
        return count;
    }

    public static boolean isDominate(boolean[][] array, int j, int i) {
            int length = array[0].length;
            boolean is = true;
            for (int l = 0; l < length - 1; l++) {
                if (array[j][l] && !array[i][l]) {
                    return false;
                }
            }
            return true;
    }

}
