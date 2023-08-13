import java.util.*;

public class task14 {
    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);
        int n = scan.nextInt();
        int[] vector = new int[n];
        for (int i = 0; i < n; i++) {
            vector[i] = scan.nextInt();
        }
        scan.close();

        List<Integer> list = new ArrayList<>();
        for (int i = 1; i <= n; i++) {
            list.add(i);
        }

        long result = 0;
        int num = n;
        for (int i = 0; i < n; i++) {
            num--;
            long numFact = fact(num);
            for (int j = 0; j < list.size(); j++) {
                if (vector[i] == list.get(j)) {
                    result += numFact * j;
                    list.remove(j);
                    break;
                }
            }
        }
        System.out.println(result);
    }

    public static long fact(int n) {
        if (n == 0) {
            return 1;
        }

        return n * fact(n  -1);
    }


}
