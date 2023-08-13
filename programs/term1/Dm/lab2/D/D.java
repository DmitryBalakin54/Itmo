import java.util.*;

public class D {
    public static void main(String[] args) {
        mainProgram();
    }

    public static void mainProgram() {
        Scanner scan = new Scanner(System.in);
        StringBuilder str = new StringBuilder(scan.next());
        scan.close();
        char[] dictionary = new char[26];
        for (int i = 'a'; i <= 'z'; i++) {
            dictionary[i - 'a'] = (char) i;
        }
        for (int i = 0; i < str.length(); i++) {
            char ch = str.charAt(i);
            for (int j = 0; j < dictionary.length; j++) {
                if (dictionary[j] == ch) {
                    System.out.print(j + 1);
                    System.out.print(" ");
                    rebuildDictionary(j, dictionary);
                    break;
                }
            }
        }
    }

    public static void rebuildDictionary(int pos, char[] dictionary) {
        char ch = dictionary[pos];
        for (int i = pos; i > 0; i--) {
            dictionary[i] = dictionary[i - 1];
        }
        dictionary[0] = ch;
    }
}
