import java.awt.color.ICC_ColorSpace;
import java.util.*;

public class ISecond {
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
        System.out.println(coder(str));
    }

    public static void secondStart(StringBuilder str) {
        int length = str.length();
        StringBuilder sb = new StringBuilder();
        sb.append(str.charAt(length - 1));
        for (int i = length - 2; i >= 0; i--) {
            sb.append(str.charAt(i) == str.charAt(i - 1) ? '0' : '1');
        }
        System.err.println(sb);
        sb = coder(sb);
        System.err.println(sb);
        for (int i = 0; i < length; i++) {
            if (str.charAt(i) != sb.charAt(i)) {
                System.err.println(str);
                System.err.println(sb);
                str.insert(i, sb.charAt(i));
                str.delete(i, i + 1);
                System.err.println(str);
                //str.replace(i, i, "" + sb.charAt(i));
                break;
            }
        }
        System.out.println(str);
    }

//    public static StringBuilder decoder(StringBuilder str) {
//
//    }

    public static StringBuilder coder(StringBuilder str) {
        int length = str.length();
        boolean[] strToBoolean = new boolean[length];
        for (int i = 0; i < length; i++) {
            strToBoolean[i] = str.charAt(i) == '1';
        }
        char ch = str.charAt(0);
        str = new StringBuilder();
        str.append(ch);
        for (int i = 1; i < length; i++) {
            strToBoolean[i] = strToBoolean[i - 1] ^ strToBoolean[i];
            str.append(strToBoolean[i] ? '1' : '0');
        }
        return str;
    }
}
