import java.util.*;

public class I {
    public static void main(String[] args) {
        mainProgram();
    }

    public static void mainProgram() {
        Scanner scan = new Scanner(System.in);
        int n = scan.nextInt();
        StringBuilder str = new StringBuilder(scan.next());
        scan.close();
        if (n == 1) {
            firstStart(str);
        } else {
            secondStart(str);
        }

    }

    public static void firstStart(StringBuilder str) {
//        int powTwo = searchTwo(str);
//        if (str.length() == 1) {
//            System.out.println(str.append(str.charAt(0)));
//            return;
//        }
//        for (int i = 0; i + 1 < Math.log(powTwo) / Math.log(2); i += 2) {
//            str.append(str.charAt(i) == str.charAt(i + 1) ? '0' : '1');
//        }
//        for (int i = 1; i + 2 < Math.log(powTwo) / Math.log(2); i += 1) {
//            str.append(str.charAt(i) == str.charAt(i + 2) ? '0' : '1');
//        }
//        System.out.println(str);

        int length = str.length();
        for (int i = 1; i < length; i++) {
            str.append(str.charAt(i) == str.charAt(i - 1) ? '0' : '1');
        }
        str.append(str.charAt(0) == str.charAt(length - 1) ? '0' : '1');
        System.out.println(str);
    }

    public static void secondStart(StringBuilder str) {
//        int k = searchDegreeOfTwo(str.length());
//        int powTwo = (int) Math.pow(2, k);
//        for (int i = 0; i < ) {
//
//        }
        int k = str.length() / 2;
        boolean[] strToBollean = new boolean[str.length()];
        for (int i = 0; i < strToBollean.length; i++) {
            strToBollean[i] = str.charAt(i) == '1';
        }
        boolean[] xorStr = new boolean[str.length()];
        int countErrors = 0;
        int firstFlag = -1;
        int secondFlag = -1;
        for (int i = 0; i < str.length(); i++) {
            if (i < k) {
                xorStr[i] = strToBollean[i];
            } else if (i != str.length() - 1){
                xorStr[i] = strToBollean[i - k] ^ strToBollean[i - k + 1];
            } else {
                xorStr[i] = strToBollean[0] ^ strToBollean[i - k];
            }
            if (xorStr[i] != strToBollean[i]) {
                countErrors++;
                if (firstFlag == -1) {
                    firstFlag = i;
                } else {
                    secondFlag = i;
                }
            }
        }
        if (countErrors == 1) {
            System.out.println(str.substring(0, k));
        } else if (countErrors > 0) {
            if (secondFlag - firstFlag != 1) {
                System.out.print(str.charAt(0) == '1' ? '0' : '1');
                System.out.println(str.substring(1, k));
            } else {
                int errorIndex = firstFlag - k + 1;
                System.out.print(str.substring(0, errorIndex));
                System.out.print(str.charAt(errorIndex) == '1' ? '0' : '1');
                System.out.println(str.substring(errorIndex + 1, k));
            }
        } else {
            System.out.println(str.substring(0, k));
        }
    }

    public static int searchTwo(StringBuilder str) {
        int powTwo = 1;
        while (powTwo < str.length()) {
            powTwo *= 2;
        }

        while (str.length() < powTwo) {
            str.append(str.charAt(str.length() - 1) == '0' );
        }
        return powTwo;
    }

    public static int searchDegreeOfTwo(int powTwoPlusK) {
        int k = 0;
        while (k + (int) Math.pow(2, k) != powTwoPlusK) {
            k++;
        }
        return k;
    }
}
