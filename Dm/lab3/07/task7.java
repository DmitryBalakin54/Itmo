import java.util.*;

public class task7 {
    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);
        int n = scan.nextInt();
        scan.close();
        for (int i = 1; i <= n; i++) {
            int[] vector = new int[n];
            vector[0] = i;
            int value = 1;
            for (int j = 1; j < n; j++) {
                value += value == i ? 1 : 0;
                vector[j] = value;
                value++;
            }
            System.out.println();
            for () {

            }

        }
    }

    public static StringBuilder makeVector(int[] vector) {
        StringBuilder str = new StringBuilder();
        for (int j : vector) {
            str.append(j).append(" ");
        }
        return str;
    }
}
