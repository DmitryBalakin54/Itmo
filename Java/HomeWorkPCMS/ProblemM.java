import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

public class ProblemM {
    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);
        int t = scan.nextInt();
        while (t-- > 0) {
            int amount = scan.nextInt();
            int[] array = new int[amount];
            Map<Integer, Integer> a = new HashMap<>();
            while (amount > 0) {
                array[array.length - amount] = scan.nextInt();
                a.put(array[array.length - amount], 0);
                amount--;
            }

            final int MAX = 1000000000;
            int count = 0;
            for (int j = array.length - 2; j > 0; j--) {
                a.put(array[j + 1], a.get(array[j + 1]) + 1);
                for (int i = 0; i < j; i++) {
                    if (2 * array[j] - array[i] > 0 && 2 * array[j] - array[i] <= MAX) {
                        count = (a.get(2 * array[j] - array[i]) != null) ? count + a.get(2 * array[j] - array[i]) : count;
                    }
                }
            }
            System.out.println(count);
        }
        scan.close();
    }
}
