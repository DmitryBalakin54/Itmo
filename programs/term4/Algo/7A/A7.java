import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.*;

public class A7 {
    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        String[] line = reader.readLine().split(" ");
        long a = Long.parseLong(line[0]);
        long b = Long.parseLong(line[1]);
        long c = Long.parseLong(line[2]);
        reader.close();

        String result = findSolution(a, b, c);
        System.out.println(result);
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

    public static String findSolution(long a, long b, long c) {
        GCDResult gcdResult = extendedGCD(a, b);
        long gcd = gcdResult.gcd;
        long x0 = gcdResult.x;
        long y0 = gcdResult.y;

        if (c % gcd != 0) {
            return "Impossible";
        }

        x0 *= c / gcd;
        y0 *= c / gcd;

        long aDivGcd = a / gcd;
        long bDivGcd = b / gcd;
        long k;

        if (x0 <= 0) {
            k = (Math.abs(x0) + bDivGcd - 1) / bDivGcd;
        } else {
            k = -(x0 / bDivGcd);
        }

        long x = x0 + k * bDivGcd;
        long y = y0 - k * aDivGcd;

        return x + " " + y;
    }
}
