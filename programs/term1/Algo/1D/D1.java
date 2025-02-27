import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Random;

public class D1 {
    public static void main(String[] args) {
        try {
            MyScanner scan = new MyScanner(System.in);
            try {
                int n = scan.nextInt();
                scan.nextLine();
                int[] array = new int[n];
                for (int i = 0; i < n; i++) {
                    array[i] = scan.nextInt();
                }
                System.out.println(mergeSort(array, 0, n - 1));
            } catch (IOException e) {

            } finally {
                scan.close();
            }
        } catch (IOException e) {

        }


    }

    public static int[] generateRandomArray(int length) {
        int[] arr = new int[length];
        for (int i = 0; i < length; i++) {
            arr[i] =  (int) (Math.random() * 10000) % 2000 - 1000;
        }
        return arr;
    }

    public static int badIdea(int[] array) {
        int count = 0;
        for (int i = 0; i < array.length - 1; i++) {
            for (int j = i + 1; j < array.length; j++) {
                count += (array[i] > array[j]) ? 1 : 0;
            }
        }
        return count;
    }

    public static long mergeSort(int[] array, int left, int right) {
        long count = 0;
        if (right > left) {
            count += mergeSort(array, left, left + (right - left) / 2 );
            count += mergeSort(array, left + (right - left) / 2 + 1, right);
            count += merge(array, left, left + (right - left) / 2, left + (right - left) / 2 + 1, right);
        }
        return count;
    }

    public static long merge(int[] array, int leftL, int rightL, int leftR, int rightR) {
        long count = 0;
        int i = leftL;
        int j = leftR;
        int ind = 0;
        int[] arr = new int[rightR - leftL + 1];
        while (i <= rightL && j <= rightR) {
            if (array[i] <= array[j]) {
                arr[ind++] = array[i++];
            } else {
                arr[ind++] = array[j++];
                count += (leftR - i);
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