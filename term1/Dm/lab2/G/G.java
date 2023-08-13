import java.util.*;

public class G {
    public static void main(String[] args) {
        mainProgram();
    }

    public  static void mainProgram() {
        final int OFFSET = 'a';
        Scanner scan = new Scanner(System.in);
        int n = scan.nextInt();
        String str = scan.next();
        scan.close();
        int[] letters = new int[n];
        Segment[] segments = new Segment[n];
        for (char ch : str.toCharArray()) {
            letters[ch - OFFSET]++;
        }
        int length = str.length();
        int sum = 0;
        for (int i = 0; i < n; i++) {
            segments[i] = new Segment();
            segments[i].left.numerator = sum;
            segments[i].left.denominator = length;
            segments[i].right.numerator = sum + letters[i];
            segments[i].right.denominator = length;
            sum += letters[i];
        }
        Segment segment = new Segment();
        for (int i : str.toCharArray()) {
            long numerator = segment.right.numerator - segment.left.numerator;
            long denominator = length;

            segment.right.numerator = segment.left.numerator * denominator + segments[i - OFFSET].right.numerator * numerator;
            segment.left.numerator = (segments[i - OFFSET].left.numerator == 0) ? segment.left.numerator :
                    segment.left.numerator * denominator + segments[i - OFFSET].left.numerator * numerator;
            segment.right.denominator *= denominator;
            segment.left.denominator *= (segments[i - OFFSET].left.numerator == 0) ? 1 : denominator;
         }
        System.out.println(segment.getLeft() + " " + segment.getRight());
    }

    public static class Segment {
        Fraction left;
        Fraction right;

        Segment() {
            left = new Fraction();
            right = new Fraction();
            left.numerator = 0;
            left.denominator = 1;
            right.numerator = 1;
            right.denominator = 1;
        }

        public double getLeft() {
            return  (double) left.numerator / left.denominator;
        }

        public double getRight() {
            return (double) right.numerator / right.denominator;
        }
    }

    public static class Fraction{
        long numerator;
        long denominator;
    }
}
