import java.util.Scanner;
import java.lang.Math;

public class ProblemA {
    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);
        int a = scan.nextInt();
        int b = scan.nextInt();
        int n = scan.nextInt();
        scan.close();

        System.out.println((int)Math.ceil((n - b) / (b - a + 0.0)) * 2 + 1 );
    }
}
