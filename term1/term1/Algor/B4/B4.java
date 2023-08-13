import java.io.*;
import java.util.*;

public class B4 {
    public static void main(String[] args) throws IOException {
        mainProgram();
    }

    public static void mainProgram() throws IOException {
      // MyScanner scan = new MyScanner(System.in);
        Scanner scan = new Scanner(System.in);
        MyQueue queue = new MyQueue();while (true) {
           String str = scan.next();
           if (str.equals("push")) {
               int value = scan.nextInt();
               queue.push(value);
               System.out.println("ok");
           } else if (str.equals("pop")) {
               System.out.println(queue.pop());
           } else if (str.equals("front")) {
               System.out.println(queue.front());
           } else if (str.equals("clear")) {
               queue.clear();
               System.out.println("ok");
           } else if (str.equals("size")) {
               System.out.println(queue.getSize());
           } else if (str.equals("exit")) {
               System.out.println("bye");
               scan.close();
               return;
           }
           //scan.nextLine();
       }

    }


}

class MyQueue {
    private Element leftElement;
    private Element rightElement;
    private int size;

    MyQueue() {
        leftElement = null;
        rightElement = null;
        size = 0;
    }

    public void push(int value) {
        Element element = new Element(value, null);
        if (size == 0) {
           leftElement = element;
           rightElement = element;
        } else {
            rightElement.rightElement = element;
            rightElement = element;
        }
        size++;
    }

    public int pop() {
        int value = leftElement.value;
        leftElement = leftElement.rightElement;
        size--;
        return value;
    }

    public int front() {
        return leftElement.value;
    }

    public int getSize() {
        return  size;
    }

    public void clear() {
        leftElement = null;
        rightElement = null;
        size = 0;
    }
}
class Element {
    Element rightElement;
    int value;

    Element(int value, Element rightElement) {
        this.rightElement = rightElement;
        this.value = value;
    }
}

class MyScanner {

    private final int BUFFER_SIZE = 1024;

    private Reader reader;
    private final char[] buffer;
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

    private void newBuffer() throws IOException {
        len = reader.read(buffer, 0, BUFFER_SIZE);
        index = 0;
    }



    public char nextChar() throws IOException {
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

    public String next() throws IOException {
        if (hasNext()) {
            return str.toString();
        }
        throw new IOException();
    }

    public int nextInt() throws IOException {
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

    public long nextLong() throws IOException {
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

    public String nextWord() throws IOException {
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


    public void nextLine() throws IOException {
        if (!hasNextLine()) {
            throw new IOException("Line is not founded");
        }
    }




    public void close() throws IOException {
        reader.close();
    }
}
