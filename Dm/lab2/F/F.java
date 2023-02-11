import java.util.*;

public class F {
    public static void main(String[] args) {
        mainProgram();
    }

    public static void mainProgram() {
        Scanner scan = new Scanner(System.in);
        int  n = scan.nextInt();
        List<String> dictionary = new ArrayList<>();
        for (int i = 'a'; i <= 'z'; i++) {
            dictionary.add(String.valueOf((char) i));
        }
        String lastChar = dictionary.get(scan.nextInt());
        System.out.print(lastChar);
        for (int i = 1; i < n; i++) {
            int k = scan.nextInt();
            if (k >= dictionary.size()) {
                dictionary.add(lastChar + lastChar.charAt(0));
                lastChar = new String();
            }
            String ch = dictionary.get(k);
            System.out.print(ch);
            if (dictionary.contains(lastChar + ch)) {
                lastChar += ch;
            } else {
                lastChar += ch;
                for (int j = 1; j < lastChar.length(); j++) {
                   if (!dictionary.contains(lastChar.substring(0, j))) {
                       lastChar = lastChar.substring(0, j);
                       break;
                   }
                }
                dictionary.add(lastChar);
                lastChar = String.valueOf(ch);
            }
        }
        scan.close();
        //System.err.println(dictionary);
    }


}