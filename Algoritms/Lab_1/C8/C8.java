import javax.lang.model.element.Element;
import java.lang.reflect.Array;
import java.util.*;

public class C8 {
    public static void main(String[] args) {
        //mainProgram();
        //mainProgram1();
        mainProgram2();
        //mainProgram3();
    }

    public static void mainProgram() {
        Scanner scan = new Scanner(System.in);
        int n = scan.nextInt();
        int[] values = new int[n + 1];
        for (int i = 1; i <= n; i++) {
            values[i] = scan.nextInt();
        }
        int sum = scan.nextInt();
        scan.close();
        int[][] dp = new int[n + 1][sum + 1];
        for (int[] i : dp) {
            for (int j = 0; j <= sum; j++) {
                i[j] = sum + 1;
            }
        }
        for (int i = 1; i <= n; i++) {
            if (values[i] <= sum) {
                dp[i][values[i]] = 1;
            }
            for (int j = 1; j <= sum; j++) {
                dp[i][j] = Math.min(dp[i][j], dp[i - 1][j]);
                if (j - values[i] >= 0) {
                    dp[i][j] = Math.min(dp[i][j], dp[i - 1][j - values[i]] + 1);
                    dp[i][j] = Math.min(dp[i][j], dp[i][j - values[i]] + 1);
                }
            }
        }
//        for (int i = 0; i <= n; i++) {
//            System.err.println(Arrays.toString(dp[i]));
//        }
        if (dp[n][sum] != sum + 1) {
            System.out.println(dp[n][sum]);
            int column = sum;
            int string = n;
            int counter = 0;
            while (counter != dp[n][sum]) {
                if (dp[string - 1][column] == dp[string][column]) {
                    string--;
                } else if (column - values[string] >= 0) {
                    if (dp[string][column - values[string]] + 1 == dp[string][column]) {
                        System.out.print(values[string] + " ");
                        column = column - values[string];
                        counter++;
                    } else if (dp[string - 1][column - values[string]] + 1 == dp[string][column]) {
                        System.out.print(values[string - 1] + " ");
                        column = column - values[string--];
                        counter++;
                    } else {
                        System.out.print(values[string] + " ");
                        counter++;
                    }
                }
            }
        } else {
            System.out.println(-1);
        }
    }

    public static void mainProgram1() {
        Scanner scan = new Scanner(System.in);
        int n = scan.nextInt();
        int[] values = new int[n + 1];
        for (int i = 1; i <= n; i++) {
            values[i] = scan.nextInt();
        }
        int sum = scan.nextInt();
        scan.close();
        int[][] dp = new int[2][sum + 1];
        for (int i = 0; i <= sum; i++) {
            dp[0][i] = sum + 1;
            dp[1][i] = sum + 1;
        }
        for (int i = 1; i <= n; i++) {
            for (int j = 0; j <= sum; j++) {
                dp[i % 2][j] = sum + 1;
            }
            if (values[i] <= sum) {
                dp[i % 2][values[i]] = 1;
            }
            for (int j = 1; j <= sum; j++) {
                dp[i % 2][j] = Math.min(dp[i % 2][j], dp[(i - 1) % 2][j]);
                if (j - values[i] >= 0) {
                    dp[i % 2][j] = Math.min(dp[i % 2][j], dp[(i - 1) % 2][j - values[i]] + 1);
                    dp[i % 2][j] = Math.min(dp[i % 2][j], dp[i % 2][j - values[i]] + 1);
                }
            }
            //System.err.println(Arrays.toString(dp[i % 2]));
        }
        if (dp[n % 2][sum] == sum + 1) {
            System.out.println(-1);
        } else {
            System.out.println(dp[n % 2][sum]);
            int value = 0;
            Stack<Integer> stack = new Stack<>();
            for (int i = 0; i < dp[n % 2][sum]; i++) {
                stack.push(values.length - 1);
                value += values[values.length - 1];
            }
            while (value != sum) {
                int index = stack.pop();
                value -= values[index];
                if (index == 0) {
                    continue;
                } else {
                    stack.push(index - 1);
                    value += values[index - 1];
                }
                while (stack.size() < dp[n % 2][sum]) {
                    stack.push(values.length - 1);
                    value += values[values.length - 1];
                }
            }
            while (stack.size() > 0) {
                System.out.print(values[stack.pop()] + " ");
            }
        }
    }

    static class Element2 {
        int value;
        List<Boolean> way;

        public Element2(int value, List<Boolean> way) {
            this.value = value;
            this.way = way;
        }
    }

    public static void mainProgram2() {
        Scanner scan = new Scanner(System.in);
        int n = scan.nextInt();
        int[] values = new int[n + 1];
        for (int i = 1; i <= n; i++) {
            values[i] = scan.nextInt();
        }
        int sum = scan.nextInt();
        scan.close();
        Element2[][] dp = new Element2[2][sum + 1];
        for (int i = 0; i <= sum; i++) {
            dp[0][i] = new Element2(sum + 1, new ArrayList<>());
            dp[1][i] = new Element2(sum + 1, new ArrayList<>());
            dp[0][i].way.add(false);
            dp[1][i].way.add(false);
        }
        for (int i = 1; i <= n; i++) {
            for (int j = 0; j <= sum; j++) {
                dp[i % 2][j].value = sum + 1;
                dp[i % 2][j].way = new ArrayList<>();
                dp[i % 2][j].way.add(true);
            }
            if (values[i] <= sum) {
                dp[i % 2][values[i]].value = 1;
                dp[i % 2][values[i]].way = new ArrayList<>();
                dp[i % 2][values[i]].way.add(true);
            }
                for (int j = 1; j <= sum; j++) {
                    if (dp[(i - 1) % 2][j].value < dp[i % 2][j].value) {
                        dp[i % 2][j].value = dp[(i - 1) % 2][j].value;
                        dp[i % 2][j].way = dp[(i - 1) % 2][j].way;//dp[(i - 1) % 2][j].way << 1;
                        dp[i % 2][j].way.add(false);
                    }
                    if (j - values[i] >= 0) {
                        if (dp[i % 2][j - values[i]].value < dp[i % 2][j].value) {
                            dp[i % 2][j].value = dp[i % 2][j - values[i]].value + 1;
                            dp[i % 2][j].way = new ArrayList<>(dp[i % 2][j - values[i]].way);// = (dp[i % 2][j - values[i]].way << 1) + 1;
                            dp[i % 2][j].way.add(true);
                        }
                    }
                    //System.err.println(j + " " + dp[i % 2][j].way);
                }

//                for (int j = 0; j <= sum; j++) {
//                    System.err.print(dp[i % 2][j].value + " ");
//                }
//                System.err.println();
        }
            if (dp[n % 2][sum].value == sum + 1) {
                System.out.println(-1);
            } else {
                System.out.println(dp[n % 2][sum].value);
                //StringBuilder str = new StringBuilder(Long.toBinaryString(dp[n % 2][sum].way));
                List<Boolean> str = dp[n % 2][sum].way;
                //System.err.println(str);
                int string = n;
                for (int i = str.size() - 1; i >= 0; i--) {
                    if (!str.get(i)) {
                        string--;
                    } else if (str.get(i)) {
                        System.out.print(values[string] + " ");
                    }
                }
            }
        }
//    static class WayCode {
//        boolean horizontalOffset;
//        boolean verticalOffset;
//
//        public WayCode(boolean horizontalOffset, boolean verticalOffset) {
//            this.horizontalOffset = horizontalOffset;
//            this.verticalOffset = verticalOffset;
//        }
//    }
//    static class Element {
//        List<WayCode> way;
//        int value;
//
//        public Element(List<WayCode> way, int value) {
//            this.way = way;
//            this.value = value;
//        }
//    }

//    public static void mainProgram3() {
//        Scanner scan = new Scanner(System.in);
//        int n = scan.nextInt();
//        int[] values = new int[n];
//        int min = 10000001;
//        boolean dva = true;
//        for (int i = 0; i < n; i++) {
//            values[i] = scan.nextInt();
//            min = Math.min(min, values[i]);
//            if (values[i] % 2 == 1) {
//                dva = false;
//            }
//        }
//        int sum = scan.nextInt();
//        scan.close();
//        if (sum % 2 == 1 && dva) {
//            System.out.println(-1);
//            return;
//        }
//
//        int[] terms = new int[sum / min + 1];
//        int index = 0;
//        for (int i = 1; i < terms.length; i++) {
//            terms = new int[terms.length];
//            index = i;
//            int value = i * values[0];
//
//            wh : while (true) {
//                if (value == sum) {
//                    break;
//                }
//                for (int j = i - 1; j >=0; j--) {
//                    if (value == sum) {
//                    break;
//                    }
//                    if (terms[j] == values.length - 1 && j == 0) {
//                        break wh;
//                    } else if (terms[j] < values.length - 1) {
//                        value -= values[terms[j]];
//                        terms[j]++;
//                        value += values[terms[j]];
//                        break;
//                    } else if (terms[j] == values.length - 1) {
//                        value -= values[terms[j]];
//                        terms[j] = 0;
//                        value += values[terms[j]];
//                    }
//                }
//            }
//            if (value == sum) {
//                System.out.println(index);
//                for (int j = 0; j < index; j++) {
//                    System.out.print(values[terms[j]] + " ");
//                }
//                return;
//            }
//        }
//        System.out.println(-1);
//    }

}

