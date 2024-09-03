import java.util.Scanner;

public class C3 {

    static final double AMOUNT = 100;
    public static void main(String[] args) {
     mainProgram();
    }

    public static void mainProgram() {
        Scanner scan = new Scanner(System.in);
        int n = scan.nextInt();
        double a = Double.parseDouble(scan.next());
        double[] h = new double[n];
        h[0] = a;

        double left = 0;
        double right = 2 * a;
        next : for (int i = 0; i < AMOUNT; i++) {
            h[1] = (left + right) / 2;
            for (int j = 2; j < n; j++) {
                h[j] = h(h[j - 1], h[j - 2]);
                if (h[j] <= 0) {
                    left = h[1];
                    continue next;
                }
            }
            right = h[1];
        }
        System.out.printf("%.2f", h[n - 1]);
    }

    public static double h(double last, double lastLast) {
        return 2 * last - lastLast + 2;
    }
}
