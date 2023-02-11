import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;

public class E1 {
    static int [] bigArray;
    static int countS = 0;
    public static void main(String[] args) {
        //mainProgram();
        mainProgram2();
    }

    public  static void mainProgram() {
        try {
            MyScanner scan = new MyScanner(System.in);
            try {
                int n = scan.nextInt();
                long k = scan.nextLong();
                scan.nextLine();
                long [] array = new long[n];
                for (int i = 0; i < n; i++) {
                    array[i] = scan.nextLong();
                }
                long[] sum = new long[n + 1];
                sum[0] = 0;
                for (int i = 0; i < n; i++) {
                    sum[i + 1] = sum[i] + array[i];
                }
                System.out.println(mergeSort(sum, 0, n, k));
            } catch (IOException e) {

            } finally {
                scan.close();
            }
        } catch (IOException e) {

        }
    }

    public static long mergeSort(long[] array, int left, int right, long k) {
        long count = 0;
        if (right > left) {
            count += mergeSort(array, left, left + (right - left) / 2 , k);
            count += mergeSort(array, left + (right - left) / 2 + 1, right, k);
            count += merge(array, left, left + (right - left) / 2, left + (right - left) / 2 + 1, right, k);
        }
        return count;
    }

    public static long merge(long[] array, int leftL, int rightL, int leftR, int rightR, long k) {
        long count = 0;
        int i = leftL;
        int j = leftR;
        int ind = 0;
        long[] arr = new long[rightR - leftL + 1];
        while (i <= rightL && j <= rightR) {
            if (array[i] <= array[j]) {
                arr[ind++] = array[i++];
                int y = j;
                while (array[y] < array[i - 1] + k) {
                    y++;
                    if (y == rightR + 1) {
                        break;
                    }
                 }
                count += (rightR + 1 - y);
//                if (array[j] >= array[i - 1] + k) {
//                    count += (rightR - j + 1);
//                }
            } else {
                arr[ind++] = array[j++];
            }
        }
        while (i <= rightL) {
            arr[ind++] = array[i++];
        }
        while (j <= rightR) {
            arr[ind++] = array[j++];
        }
        ind = 0;
        for (int g = leftL; g <= rightR; g++) {
            array[g] = arr[ind++];
        }
        return count;
    }

    public static int badVariant(int[] array, int k) {
        int count = 0;
        int[] sum = new int[array.length + 1];
        sum[0] = 0;

        for (int i = 0; i < array.length; i++) {
            sum[i + 1] = sum[i] + array[i];
        }

        for (int j = 0; j < array.length; j++) {
            for (int i = j + 1; i < array.length + 1; i++) {
                count += (sum[i] - sum[j] >= k) ? 1 : 0;
            }
        }
        return count;
    }

    public static int badVariant2(int[] array, int k) {
        int count = 0;
        int[] sum = new int[array.length + 1];
        sum[0] = 0;

        for (int i = 0; i < array.length; i++) {
            sum[i + 1] = sum[i] + array[i];
        }
        int sumAll = sum[sum.length - 1];
        for (int j = 0; j < array.length; j++) {
            if (j >= array.length - j) {
                break;
            }
            for (int i = j + 1; i < array.length + 1 - j; i++) {
                //System.err.print("j = " + j + " i = " + i + " ind = " + (array.length + 1 - j) + " sumAll = " + sumAll);
                count += (sum[i] - sum[j] >= k) ? 1 : 0;
                if (i < array.length - j) {
                    count += (sumAll - (sum[i] - sum[j]) >= k) ? 1 : 0;
                }
                //System.err.println(" res = " + (sumAll - (sum[i] - sum[j])));
            }
            sumAll -= array[array.length - j - 1] + array[j];
        }
        return count;
    }

    public  static void mainProgram2() {
        try {
            MyScanner scan = new MyScanner(System.in);
            try {
                int n = scan.nextInt();
                long k = scan.nextLong();
                scan.nextLine();
                long [] array = new long[n];
                for (int i = 0; i < n; i++) {
                    array[i] = scan.nextLong();
                }
                long[] sum = new long[n];
                sum[0] = array[0];
                for (int i = 1; i < n; i++) {
                    sum[i] = sum[i - 1] + array[i];
                }
                System.out.println(mergeSort2(sum, 0, n - 1, k));
            } catch (IOException e) {

            } finally {
                scan.close();
            }
        } catch (IOException e) {

        }
    }

    public static long mergeSort2(long[] array, int left, int right, long k) {
        long count = 0;
        if (right > left) {
            count += mergeSort2(array, left, left + (right - left) / 2 , k);
            count += mergeSort2(array, left + (right - left) / 2 + 1, right, k);
            count += merge2(array, left, left + (right - left) / 2, left + (right - left) / 2 + 1, right, k);
        } else {
            count +=  (array[left] >= k) ? 1 : 0;
        }
        return count;
    }

    public static long merge2(long[] array, int leftL, int rightL, int leftR, int rightR, long k) {
        long count = 0;
        int i = leftL;
        int j = leftR;
        int ind = 0;
        long[] arr = new long[rightR - leftL + 1];
        while (i <= rightL && j <= rightR) {
            if (array[i] <= array[j]) {
                arr[ind++] = array[i++];
                int y = j;
                while (array[y] < array[i - 1] + k) {
                    y++;
                    if (y == rightR + 1) {
                        break;
                    }
                }
                count += (rightR + 1 - y);
//                if (array[j] >= array[i - 1] + k) {
//                    count += (rightR - j + 1);
//                }
            } else {
                arr[ind++] = array[j++];
            }
        }
        while (i <= rightL) {
            arr[ind++] = array[i++];
        }
        while (j <= rightR) {
            arr[ind++] = array[j++];
        }
        ind = 0;
        for (int g = leftL; g <= rightR; g++) {
            array[g] = arr[ind++];
        }
        return count;
    }
}

class MyScanner {

    private final int BUFFER_SIZE = 1024;

    private Reader reader;
    private char[] buffer;
    private int len;
    private int index;
    private StringBuilder str;
    private ArrayList<String> words;
    private int numOfWord;
    private char ch;
    private final char SEPARATOR;


    public MyScanner(Reader r ) throws IOException { // Main Constructor
        reader = r;
        words = new ArrayList<String>();
        numOfWord = 0;
        buffer = new char[BUFFER_SIZE];
        str = new StringBuilder();
        SEPARATOR = (System.lineSeparator().equals("\r")) ? '\r' : '\n';
        newBuffer();
    }

    public MyScanner(String str) throws IOException {
        this( (new StringReader(str)));
    }

    public MyScanner(InputStream s) throws IOException {
        this(new InputStreamReader(s));
    }

    public MyScanner(InputStream s, String encod) throws UnsupportedEncodingException, IOException{
        this(new InputStreamReader(  s, encod));
    }

    void newBuffer() throws IOException {
        len = reader.read(buffer, 0, BUFFER_SIZE);
        index = 0;
    }



    char nextChar() throws IOException {
        if (len == index) {
            newBuffer();
        }
        if (len == -1) {
            throw new IOException();
        }
        return buffer[index++];
    }

    private void newToken() throws IOException {
        str.setLength(0);
        missWhiteSpace();
        while (true) {
            ch = nextChar();
            if (Character.isWhitespace(ch)) {
                index--;
                break;
            }
            str.append(ch);
        }
    }


    private void missWhiteSpace() throws IOException {
        while (true) {
            ch = nextChar();
            if (!Character.isWhitespace(ch) || ch == SEPARATOR) {
                index--;
                break;
            }
        }
    }


    private  boolean hasNext() throws IOException {
        newToken();
        if ( (ch == SEPARATOR) && str.length() == 0) {
            index++;
            return false;
        }
        return true;
    }

    String next() throws IOException {
        if (hasNext()) {
            return str.toString();
        }
        throw new IOException();
    }

    int nextInt() throws IOException {
        if (hasNext()) {
            try {
                return  Integer.parseInt(str.toString());
            } catch (NumberFormatException e ) {
                throw new IOException("Element is not integer number");
            }
        } else {
            throw new IOException("Element is not founded");
        }
    }

    long nextLong() throws IOException {
        if (hasNext()) {
            try {
                return  Long.parseLong(str.toString());
            } catch (NumberFormatException e ) {
                throw new IOException("Element is not integer number");
            }
        } else {
            throw new IOException("Element is not founded");
        }
    }
    String nextWord() throws IOException {
        if ( (words.size() == 0 || numOfWord >= words.size() ))  {
            if (!hasNext()) {
                throw new IOException("Word is not founded");
            }
            numOfWord = 0;
            int startSubstr = 0;
            words = new ArrayList<String>();
            for (int i = 0; i <= str.length(); i++) {
                if (i != str.length()) {
                    if (!(Character.isLetter(str.charAt(i)) ||  str.charAt(i) == '\'' || Character.getType(str.charAt(i)) == Character.DASH_PUNCTUATION)) {
                        if (startSubstr - i < 0) {
                            words.add(str.substring(startSubstr, i));
                            startSubstr = i + 1;
                        } else {
                            startSubstr++;
                        }
                    }
                } else if ((Character.isLetter(str.charAt(i - 1)) ||  str.charAt(i - 1) == '\'' || Character.getType(str.charAt(i - 1)) == Character.DASH_PUNCTUATION)) {
                    if (startSubstr - i < 0) {
                        words.add(str.substring(startSubstr, i));
                    }
                }
            }

            if (words.size() == 0) {
                if (str.length() == 0) {
                    throw new IOException("Word is not founded");
                } else {
                    return nextWord();
                }
            } else {
                numOfWord++;
                return words.get(numOfWord - 1);
            }

        } else {
            numOfWord++;
            return words.get(numOfWord - 1);
        }
    }


    private boolean hasNextLine() throws IOException {
        index--;
        while (true) {
            ch = nextChar();
            if (ch == SEPARATOR) {
                try {
                    ch = nextChar();
                } catch (IOException e) {
                    return false;
                }
                index--;
                return true;
            }
        }
    }


    void nextLine() throws IOException {
        if (!hasNextLine()) {
            throw new IOException("Line is not founded");
        }
    }




    void close() throws IOException {
        reader.close();
    }
}