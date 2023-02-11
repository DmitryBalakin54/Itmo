import java.util.*;

public class task13 {
    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);
        int n = scan.nextInt();
        long k = scan.nextLong();
        scan.close();
        List<Integer> list = new ArrayList<>();
        for (int i = 1; i <= n; i++) {
            list.add(i);
        }

        StringBuilder result = new StringBuilder();
        int num = n;
        for (int i = 0; i < n - 1; i++) {
            num--;
            long numFact = fact(num);
            long j = (k / numFact) * numFact;

//            System.err.println(j + " " + num + " " + k);
//            System.err.println(list);
            result.append(list.get((int) (j / numFact))).append(" ");
            list.remove( (int) (j / numFact));
            k %= numFact;
        }
        result.append(list.get(0));
        System.out.println(result);
    }

    public static long fact(int n) {
        long result = 1;
        for (int i = 1; i <= n; i++) {
            result *= i;
        }
        return result;
    }

}
