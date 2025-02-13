import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;

public class B7 {
    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        int n = Integer.parseInt(reader.readLine().trim());
        for (int i = 0; i < n; i++) {
            String[] inputs = reader.readLine().trim().split(" ");
            long a = Long.parseLong(inputs[0]);
            long b = Long.parseLong(inputs[1]);
            long nValue = Long.parseLong(inputs[2]);
            long m = Long.parseLong(inputs[3]);
            System.out.println(solveCongruences(a, b, nValue, m));
        }
    }

    public static class GCDResult {
        long gcd, x, y;

        GCDResult(long gcd, long x, long y) {
            this.gcd = gcd;
            this.x = x;
            this.y = y;
        }
    }

    public static GCDResult extendedGCD(long a, long b) {
        if (b == 0) {
            return new GCDResult(a, 1, 0);
        }
        GCDResult result = extendedGCD(b, a % b);
        long x = result.y;
        long y = result.x - (a / b) * result.y;
        return new GCDResult(result.gcd, x, y);
    }

    public static String solveCongruences(long a, long b, long n, long m) {
        GCDResult gcdResult = extendedGCD(n, m);
        long gcd = gcdResult.gcd;
        long x0 = gcdResult.x;
        long y0 = gcdResult.y;

        // Check for compatibility
        if ((a - b) % gcd != 0) {
            return "NO";
        }

        // Find the particular solution using the extended gcd results
        long lcm = n * (m / gcd);
        long k = (b - a) / gcd;
        long x = (a + k * x0 * n) % lcm;

        if (x < 0) {
            x += lcm;
        }

        return "YES " + x + " " + lcm;
    }
}
