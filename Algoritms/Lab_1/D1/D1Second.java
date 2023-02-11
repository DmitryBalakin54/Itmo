import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;

public class D1Second {
    static int count = 0;
    public static void main(String[] args) {
        try {
            MyScanner scan = new MyScanner(System.in);
            try {
                int n = scan.nextInt();
                scan.nextLine();
                int[] array = new int[n];
                for (int i = 0; i < n; i++) {
                    array[i] = scan.nextInt();
                }
                mergeSort(array, 0, n - 1);
                System.out.println(count);
            } catch (IOException e) {

            } finally {
                scan.close();
            }
        } catch (IOException e) {

        }
    }

    public static void mergeSort(int[] array, int left, int right) {
        if (right > left) {
            mergeSort(array, left, left + (right - left) / 2 );
            mergeSort(array, left + (right - left) / 2 + 1, right);
            merge(array, left, left + (right - left) / 2, left + (right - left) / 2 + 1, right);
        }
    }

    public static void merge(int[] array, int leftL, int rightL, int leftR, int rightR) {
        int i = leftL;
        int j = leftR;
        int ind = 0;
        int[] arr = new int[rightR - leftL + 1];
        while (i <= rightL && j <= rightR) {
            if (array[i] <= array[j]) {
                arr[ind++] = array[i++];
            } else {
                arr[ind++] = array[j++];
                count += (leftR - i);
            }
        }
        while (i <= rightL) {
            arr[ind++] = array[i++];
        }
        while (j <= rightR) {
            arr[ind++] = array[j++];
        }
        ind = 0;
        for (int g = leftL; g <= rightR; g++) {
            array[g] = arr[ind++];
        }
    }
}