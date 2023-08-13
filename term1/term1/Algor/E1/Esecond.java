import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;

public class Esecond {
    public static void main(String[] args) {
        try {
            MyScanner scan = new MyScanner(System.in);
            try {
                int n = scan.nextInt();
                int k = scan.nextInt();
                scan.nextLine();
                int[] array = new int[n];

                for (int i = 0; i < n; i++) {
                    array[i] = scan.nextInt();
                }
                int[] sum = new int[n + 1];
                sum[0] = 0;
                for (int i = 0; i < n; i++) {
                    sum[i + 1] = sum[i] + array[i];
                }
                System.out.println(mergeSort(sum, 0, n + 1, k));
            } catch (IOException e) {

            } finally {
                scan.close();
            }
        } catch (IOException e) {

        }
    }

    public static int mergeSort(int[] a, int left, int right, int k) {
        int count = 0;
        if (left + 1 >= right) {
            return count;
        }
        int mid = (left + right) / 2;
        count += mergeSort(a, left, mid, k);
        count += mergeSort(a, mid, right, k);
        count += merge(a, left, mid, right, k);
        return count;
    }

    public static int merge(int[] a, int left, int mid, int right, int k) {
        int count = 0;

        int it1 = 0;
        int it2 = 0;
        int[] result = new int[right - left];

        while (left + it1 < mid & mid + it2 < right) {
            if (a[left + it1] < a[mid + it2]) {
                result[it1 + it2] = a[left + it1];
                int y = mid + it2;
                while (a[y] < a[left + it1] + k) {
                    y++;
                    if (y == right) {
                        break;
                    }
                }
                count += right - y;
                it1 += 1;
            } else {
                result[it1 + it2] = a[mid + it2];
                it2 += 1;
            }
        }

        while (left + it1 < mid) {
            result[it1 + it2] = a[left + it1];
            it1 += 1;
        }

        while (mid + it2 < right) {
            result[it1 + it2] = a[mid + it2];
            it2 += 1;
        }

        for (int i = 0; i < it1 + it2; i++) {
            a[left + i] = result[i];
        }
        return count;
    }

}
