import java.util.*;

public class IThird {
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
        str = BW(str.toString());
        str = LZW(str.toString());
        int length = str.length();
        for (int i = 1; i < length; i++) {
            str.append(str.charAt(i) == str.charAt(i - 1) ? '0' : '1');
        }
        str.append(str.charAt(0) == str.charAt(length - 1) ? '0' : '1');
        str = 
        System.out.println(str);
    }

    public static void secondStart(StringBuilder str) {
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



    public  static StringBuilder BW(String code) {
        StringBuilder str = new StringBuilder(code);
        StringBuilder sb = new StringBuilder();
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
                sb.append(s.charAt(s.length() - 1));
            }
        }
        return sb;
    }


    public static StringBuilder LZW(String code) {
        String str = code;
        StringBuilder sb = new StringBuilder();
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
                    sb.append(lastStringNum).append(" ");
                    index += dictionary.get(lastStringNum).length();
                    //index++;
                    break;
                }
                String substring;
                if ((stringNum = searchSubstring(
                        substring = str.substring(index, index + i + 2), dictionary)) == -1)
                {
                    sb.append(lastStringNum).append(" ");
                    dictionary.add(substring);
                    maxLength = Math.max(substring.length(), maxLength);
                    index += substring.length() - 1;
                    break;
                } else {
                    lastStringNum = stringNum;
                }
            }
        }
        return sb;
    }

    public static int searchSubstring(String substring, List<String> dictionary) {
        for (int i = 0; i < dictionary.size(); i++) {
            if (substring.equals(dictionary.get(i))) {
                return i;
            }
        }
        return -1;
    }


    public  static  StringBuilder reverseBW(String code) {
        String str = code;
        StringBuilder sb = new StringBuilder();
        List<StringBuilder> list = new ArrayList<>();
        for (int i = 0; i < str.length(); i++) {
            list.add(new StringBuilder());
        }
        for (int i = 0; i < str.length(); i++) {
            append(list, str);
            list.sort(null);
        }
        sb = list.get(0);
        return sb;

    }

    public static void append(List<StringBuilder> list, String str) {
        for (int j = 0; j < list.size(); j++) {
            list.get(j).insert(0, str.charAt(j));
        }
    }

    public static StringBuilder reverseLZW(String code) {
        Scanner scan = new Scanner(code);
        int  n = scan.nextInt();
        List<String> dictionary = new ArrayList<>();
        StringBuilder sb = new StringBuilder();
        for (int i = 'a'; i <= 'z'; i++) {
            dictionary.add(String.valueOf((char) i));
        }
        String lastChar = dictionary.get(scan.nextInt());
        sb.append(lastChar);
        for (int i = 1; i < n; i++) {
            int k = scan.nextInt();
            if (k >= dictionary.size()) {
                dictionary.add(lastChar + lastChar.charAt(0));
                lastChar = new String();
            }
            String ch = dictionary.get(k);
            sb.append(ch);
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
        return sb;
    }
}
