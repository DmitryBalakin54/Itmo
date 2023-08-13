import java.util.List;
import java.util.Scanner;

public class D4 {
    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);
        int n = scan.nextInt();
        scan.close();

        List<String> list = List.of("AaAaAa", "AaAaBB", "AaBBAa", "AaBBBB", "BBAaBB", "BBBBAa", "BBBBBB");
        int[] p = {0, 1, 2, 3, 4, 5, 6};
        for (int i = 0; i < n; i++) {
            for (int j : p) {
                System.out.print(list.get(j));
            }
            System.out.println();
            nextPermutation(p);
        }
    }

    static void nextPermutation(int[] array) {
        for (int left = array.length - 2; left >= 0; left--) {
            if (array[left] < array[left + 1]) {
                for (int right = array.length - 1; ; right--) {
                    if (array[right] > array[left]) {
                        swap(array, left, right);

                        left++;
                        right = array.length - 1;

                        while (left < right) {
                            swap(array, left, right);
                            left++;
                            right--;
                        }

                        return;
                    }
                }
            }
        }
    }

    static void swap(int[] array, int ind1, int ind2) {
        if (ind1 == ind2) {
            return;
        }
        int tmp = array[ind1];
        array[ind1] = array[ind2];
        array[ind2] = tmp;
    }
}
