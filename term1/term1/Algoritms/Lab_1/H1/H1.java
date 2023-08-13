import java.util.Arrays;
import java.util.Scanner;

public class H1 {
    static final int SIZE = 26;
    public static void main(String[] args) {
        mainProgram();
    }

    public static void mainProgram() {
        Scanner scan = new Scanner(System.in);
        int n = scan.nextInt();
        int m = scan.nextInt();
        int k = scan.nextInt();

        StringBuilder[] strings = new StringBuilder[n];
        for (int i = 0; i < n; i++) {
            strings[i] = new StringBuilder(scan.next());
        }
        digitSort(strings, k);
        for (StringBuilder i : strings) {
            System.out.println(i);
        }
    }

    public static void digitSort(StringBuilder[] array, int amount) {
        for (int i = 0; i < amount; i++) {
            int[] chars = new int[SIZE];
            int[] beginIndex = new int[SIZE];
            for (int j = 0; j < array.length; j++) {
                //System.err.println(array[j].length() - 1 - i);
                chars[array[j].charAt(array[j].length() - 1 - i) - 'a'] ++;
            }
            for (int j = 1; j < beginIndex.length; j++) {
                beginIndex[j] = beginIndex[j - 1] + chars[j - 1];
            }
            StringBuilder[] res = new StringBuilder[array.length];
            for (int j = 0; j < array.length; j++) {
                int index = beginIndex[array[j].charAt(array[j].length() - 1 - i) - 'a']++;
                res[index] = array[j];
            }
//            for (StringBuilder st : res) {
//                System.err.println(st);
//            }
            for (int j = 0; j < array.length; j++) {
                array[j] = res[j];
            }
        }
    }
}
