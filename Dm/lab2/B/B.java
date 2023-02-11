import java.util.*;

public class B {
    public static void main(String[] args) {
        mainProgram();
    }

    public  static void mainProgram() {
        Scanner scan = new Scanner(System.in);
        StringBuilder str = new StringBuilder(scan.nextLine());
        scan.close();
        for (int i = 'a'; i <= 'z'; i++) {
            String string = String.valueOf((char) i);
            int ind = 0;
            List<String> list = new ArrayList<>();
            int k;
            while ( (k = str.indexOf(string, ind)) > -1) {
                list.add(str.substring(k, str.length()) +
                        ((k > 0) ? str.substring(0, k) : ""));
                ind = ++k;
            }
            list.sort(null);
            for (String s : list) {
                System.out.print(s.charAt(s.length() - 1));
            }
        }

    }
}
