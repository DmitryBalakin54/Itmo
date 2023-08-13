import java.util.Scanner;

public class Egit {

    private static long inversions = 0;

    private static void merge(int[] a, int leftStart, int mid, int rightEnd, int k) {
        int i = 0;
        int j = 0;
        int leftLength = mid - leftStart;
        int rightLength = rightEnd - mid;
        while (i < leftLength || j < rightLength) {
            if (i == leftLength) {
                while (j < rightLength) {
                    inversions += leftLength - i;
                    j++;
                }
                break;
            } else if (j == rightLength) {
                while (i < leftLength) {
                    i++;
                }
                break;
            }
            if (a[i + leftStart] < a[j + mid] + k) {
                i++;
            } else {
                inversions += leftLength - i;
                j++;
            }
        }
        i = 0;
        j = 0;
        long[] buffer = new long[rightEnd - leftStart];
        while (i < leftLength || j < rightLength) {
            if (i == leftLength) {
                while (j < rightLength) {
                    buffer[i + j] = a[j + mid];
                    j++;
                }
                break;
            } else if (j == rightLength) {
                while (i < leftLength) {
                    buffer[i + j] = a[i + leftStart];
                    i++;
                }
                break;
            }
            if (a[i + leftStart] <= a[j + mid]) {
                buffer[i + j] = a[i + leftStart];
                i++;
            } else {
                buffer[i + j] = a[j + mid];
                j++;
            }
        }
//        System.arraycopy(buffer, 0, a, leftStart, buffer.length);
    }

    private static long countInv(int[] a, int k) {
        sort(a, 0, a.length, k);
        return inversions;
    }

    private static void sort(int[] a, int left, int right, int k) {
        int aLength = right - left;
        if (aLength == 1) {
            if (a[left] >= k) {
                inversions++;
            }
            return;
        }
        int mid = aLength / 2;
        sort(a, left, mid + left, k);
        sort(a, mid + left, right, k);
        merge(a, left, mid + left, right, k);
    }

    private static void swap(long[] a, int i, int j) {
        long temp = a[i];
        a[i] = a[j];
        a[j] = temp;
    }

    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);
        int n = scan.nextInt();
        int k = scan.nextInt();
        int[] a = new int[n];
        for ( int i = 0; i < n; i++) {
            a[i] = scan.nextInt();
        }
        scan.close();
        System.out.println(countInv(a, k));
    }

}