import java.util.Arrays;
import java.util.Scanner;

public class D {
    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);
        int n = scan.nextInt();
        int[] array = new int[n];
        for (int i = 0; i < n; i++) {
            array[i] = scan.nextInt();
        }
        int[] subarray = Arrays.copyOf(array, array.length);
        System.out.println(mergesort(array, subarray, 0, n - 1));
    }
    public static int merge(int[] array, int[] subarray, int left, int mid, int right) {
        int k = left, i = left, j = mid + 1;
        int count = 0;
        while (i <= mid && j <= right) {
            if (array[i] <= array[j]) {
                subarray[k++] = array[i++];
            } else {
                subarray[k++] = array[j++];
                count += (mid - i + 1);
            }
        }
        while (i <= mid) {
            subarray[k++] = array[i++];
        }
        for (i = left; i <= right; i++) {
            array[i] = subarray[i];
        }
        return count;
    }

    public static int mergesort(int[] array, int[] subarray, int left, int right) {
        if (right <= left) {
            return 0;
        }
        int mid = (left + ((right - left) >> 1));
        int count = 0;
        count += mergesort(array, subarray, left, mid);
        count += mergesort(array, subarray, mid + 1, right);
        count += merge(array, subarray, left, mid, right);
        return count;
    }
}
