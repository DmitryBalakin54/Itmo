import java.io.*;
import java.util.Scanner;
import java.util.ArrayList;
import java.util.Arrays;

public class Eeeee {
    private static long amount = 0;

    private static void merge(long[] array, int left, int mid, int right, long k) {
        int i = 0;
        int j = 0;
        int lengthL = mid - left;
        int lengthR = right - mid;
        while (i < lengthL || j < lengthR) {
            if (i == lengthL) {
                while (j < lengthR) {
                    amount += lengthL - i;
                    j++;
                }
                break;
            } else if (j == lengthR) {
                while (i < lengthL) {
                    i++;
                }
                break;
            }
            if (array[i + left] < array[j + mid] + k) {
                i++;
            } else {
                amount += lengthL - i;
                j++;
            }
        }
        i = 0;
        j = 0;
        long[] arr = new long[right - left];
        while (i < lengthL || j < lengthR) {
            if (i == lengthL) {
                while (j < lengthR) {
                    arr[i + j] = array[j + mid];
                    j++;
                }
                break;
            } else if (j == lengthR) {
                while (i < lengthL) {
                    arr[i + j] = array[i + left];
                    i++;
                }
                break;
            }
            if (array[i + left] <= array[j + mid]) {
                arr[i + j] = array[i + left];
                i++;
            } else {
                arr[i + j] = array[j + mid];
                j++;
            }
        }
        System.arraycopy(arr, 0, array, left, arr.length);
    }

    private static long count(long[] a, long k) {
        mergeSort(a, 0, a.length, k);
        return amount;
    }

    private static void mergeSort(long[] a, int left, int right, long k) {
        int aLength = right - left;
        if (aLength == 1) {
            amount += (a[left] >= k) ? 1 : 0;
            return;
        }
        int mid = aLength / 2;
        mergeSort(a, left, mid + left, k);
        mergeSort(a, mid + left, right, k);
        merge(a, left, mid + left, right, k);
    }

    private static void swap(long[] array, int i, int j) {
        long a = array[i];
        array[i] = array[j];
        array[j] = a;
    }

    public static void main(String[] args) throws IOException {
        MyScanner scan = new MyScanner(System.in);
        int n = scan.nextInt();
        long k = scan.nextLong();
        scan.nextLine();
        long [] array = new long[n];
        for (int i = 0; i < n; i++) {
            array[i] = scan.nextLong();
        }
        scan.close();
        long[] sum = new long[array.length];
        sum[0] = array[0];
        for (int i = 1; i < sum.length; i++) {
            sum[i] = sum[i - 1] + array[i];
        }
        for (int i = 0; i < sum.length / 2; i++) {
            swap(sum, i, sum.length - i - 1);
        }
        System.out.println(count(sum, k));
    }

}