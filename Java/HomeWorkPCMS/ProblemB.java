import java.util.Scanner;

public class ProblemB {
    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);
        int n = scan.nextInt();
        scan.close();

        final int BEGIN = -710 * 25000;
        final int END = -BEGIN - 1;
        final int STEP = 2 * 710;

        while (n > 0) {
            System.out.println(BEGIN + (n - 1) * 710);
            n--;
        }


    }
}
