import java.util.*;

public class G2 {
    public static void main(String[] args) throws InterruptedException {
        mainProgram();
    }

    public  static void mainProgram() throws InterruptedException {
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
            segments[i].left = (double) sum / length;
            segments[i].right = (double) (sum + letters[i]) / length;
            sum += letters[i];
        }
        Segment segment = new Segment();
        for (int i : str.toCharArray()) {
            double range = segment.right - segment.left;
            segment.right = segment.left + segments[i - OFFSET].right * range;
            segment.left = segment.left + segments[i - OFFSET].left * range;
        }
        int[] arr = binSearch2(segment.left, segment.right);
        int numerator = arr[0];
        int q = arr[1];
        StringBuilder result = new StringBuilder(Integer.toBinaryString(numerator));
        while (result.length() < q) {
            result.insert(0, "0");
        }
        System.out.println(n);
        for (int i : letters) {
            System.out.print(i);
            System.out.print(" ");
        }
        System.out.println();
        System.out.println(result);

    }

    public static int[] binSearch2(double left, double right) throws InterruptedException {
        double mid = (right + left) / 2;
        int q = 0;
        long two = 1;
        int val = 0;
        while ( Math.floor(mid * two) / two < left ||
                 Math.floor(mid * two) / two >= right)
        {
            q++;
            two *= 2;
        }
        val = (int) Math.floor(mid * two);
        int subVAl = val;
        int bestVal = 0;
        int bestPow = 0;
        while ((double) (subVAl + 1) / two < right) {
            subVAl++;
            if ((double) subVAl / two >= left) {
                if (twoCount(subVAl) > bestPow) {
                    bestPow = twoCount(subVAl);
                    bestVal = subVAl;
                }
            }
        }
        if (bestVal > 0) {
            val = bestVal;
        }

        subVAl = val;
        bestVal = 0;
        bestPow = 0;
        while ((double) (subVAl - 1) / two >= left) {
            subVAl--;
            if ((double) subVAl / two < right) {
                if (twoCount(subVAl) > bestPow) {
                    bestPow = twoCount(subVAl);
                    bestVal = subVAl;
                }
            }
        }
        if (bestVal > 0) {
            val = bestVal;
        }
        while (val % 2 == 0 && val != 0) {
            val /= 2;
            q--;
        }
        return new int[] {val, q};
    }

   public static int[] binSearch(double left, double right) throws InterruptedException {
        int val = 0;
        int q = 0;
        long two = 1;
        while ((double) val / two < left || (double) val / two >= right) {
            if ((double) val / two < left) {
                int subVAl = val;
                int bestVal = 0;
                int bestPow = 0;
                while ((double) (subVAl + 1) / two < right) {
                    subVAl++;
                    if ((double) subVAl / two >= left) {
                        if (twoCount(subVAl) > bestPow) {
                            bestPow = twoCount(subVAl);
                            bestVal = subVAl;
                        }
                    }
                }
                if (bestVal > 0) {
                    val = bestVal;
                    break;
                }
                val = val * 2 + 1;
                two *= 2;
                q++;
            } else if ((double) val / two >= right) {
                int subVAl = val;
                int bestVal = 0;
                int bestPow = 0;
                while ((double) (subVAl - 1) / two >= left) {
                    subVAl--;
                    if ((double) subVAl / two < right) {
                        if (twoCount(subVAl) > bestPow) {
                            bestPow = twoCount(subVAl);
                            bestVal = subVAl;
                        }
                    }
                }
                if (bestVal > 0) {
                    val = bestVal;
                    break;
                }
                val = val * 2 - 1;
                two *= 2;
                q++;
            } else {
                break;
            }
//            System.out.println((double) val / two);
//            Thread.sleep(100);
        }
        return new int[] {val, q};
   }

   public static int twoCount(long num) {
        int res = 0;
        while (num % 2 == 0 && num != 0) {
            num /= 2;
            res++;
        }
        return res;
   }
    public static class Segment {
        double left;
        double right;

        Segment() {
            left = 0;
            right = 1;
        }
    }

}

