import java.util.*;

public class E {
    public static void main(String[] args) {
        mainProgram();
    }

    public static void mainProgram() {
        Scanner scan = new Scanner(System.in);
        String str = scan.next();
        scan.close();
        List<String> dictionary = new ArrayList<>();
        for (int i = 'a'; i <= 'z'; i++) {
            dictionary.add(String.valueOf((char) i));
        }
        int index = 0;
        int maxLength = 1;
        while (index < str.length()) {
            int stringNum;
            int lastStringNum = searchSubstring(str.substring(index, index + 1), dictionary);
            for (int i = 0; i < maxLength; i++) {
                if (index + i + 1 >= str.length()) {
                    System.out.print(lastStringNum);
                    System.out.print(" ");
                    index += dictionary.get(lastStringNum).length();
                    //index++;
                    break;
                }
                String substring;
                if ((stringNum = searchSubstring(
                    substring = str.substring(index, index + i + 2), dictionary)) == -1)
                {
                    System.out.print(lastStringNum);
                    System.out.print(" ");
                    dictionary.add(substring);
                    maxLength = Math.max(substring.length(), maxLength);
                    index += substring.length() - 1;
                    break;
                } else {
                    lastStringNum = stringNum;
                }
            }
        }
    }

    public static int searchSubstring(String substring, List<String> dictionary) {
        for (int i = 0; i < dictionary.size(); i++) {
            if (substring.equals(dictionary.get(i))) {
                return i;
            }
        }
        return -1;
    }
}
