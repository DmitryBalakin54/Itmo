import java.util.*;

public class A3 {
    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);
        int n = scan.nextInt();
        int k = scan.nextInt();
        int[] sortedArray = new int[n];
        int[] numbers = new int[k];
        for (int i = 0; i < n; i++) {
            sortedArray[i] = scan.nextInt();
        }
        for (int i = 0; i < k; i++) {
            numbers[i] = scan.nextInt();
        }
        scan.close();

        for (int i : numbers) {
            if (binSearch(sortedArray, i)) {
                System.out.println("YES");
            } else {
                System.out.println("NO");
            }
        }
    }

    public static boolean binSearch(int[] array, int value) {
        int left = 0;
        int right = array.length - 1;
        int index = (left + right) / 2;
        while (left < right) {
            if (array[index] > value) {
                right = index;
            } else if (array[index] < value) {
                left = index;
            } else {
                return true;
            }
            index = (left + right) / 2;
            if (index == left || index == right) {
                if (array[left] == value || array[right] == value) {
                    return true;
                } else {
                    return false;
                }
            }
        }
        return false;
    }
}
