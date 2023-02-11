import java.util.*;

public class Aaa {
    public static void main(String[] args) {
        mainProgram();
    }

    public static void  mainProgram() {
        Scanner scan = new Scanner(System.in);
        int n = scan.nextInt();
        long[] array = new long[n];
        for (int i = 0; i < n; i++) {
            array[i] = scan.nextLong();
        }
        scan.close();
        long sum = 0;
        while (array.length > 1) {
            Arrays.sort(array);
            long newEl = array[0] + array[1];
            sum += newEl;
            array = Arrays.copyOfRange(array, 2, array.length + 1);
            array[array.length - 1] = newEl;
        }
        System.out.println(sum);
    }


}
