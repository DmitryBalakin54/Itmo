import java.io.*;
import java.util.*;

public class A6 {
    public static void main(String[] args) throws IOException {
        //mainProgram();
        //mainProgram2();
        //mainProgram3();
        mainProgram4();
    }

    public static void mainProgram() {
        Scanner scan = new Scanner(System.in);
        int n = Integer.parseInt(scan.nextLine());
        StringBuilder str = new StringBuilder(scan.nextLine());
        scan.close();

        Deque decA = new ArrayDeque();
        Deque decB = new ArrayDeque();

        int personNumber = 1;
        StringBuilder res = new StringBuilder();
        for (int i = 0; i < n; i++) {
            char ch = str.charAt(i);
            if (ch == 'a') {
                decA.addLast(personNumber++);
            } else if (ch == 'b') {
                decB.addLast(personNumber++);
            } else if (ch == 'A') {
                res.append((int) decA.removeFirst() % 10);
            } else if (ch == 'B') {
                res.append((int) decB.removeFirst() % 10);
            } else if (ch == '>') {
                while (decA.size() > 0) {
                    decB.addLast(decA.removeLast());
                }
            } else if (ch == ']') {
                while (decB.size() > 0) {
                    decA.addLast(decB.removeLast());
                }
            } else if (ch == '<') {
                while (decA.size() + 1 < decB.size()) {
                    decA.addLast(decB.removeLast());
                }
            } else if (ch == '[') {
                while (decB.size() + 1 < decA.size()) {
                    decB.addLast(decA.removeLast());
                }
            }
        }
        System.out.println(res);
    }

    public static void mainProgram2() {
        Scanner scan = new Scanner(System.in);
        int n = Integer.parseInt(scan.nextLine());
        StringBuilder str = new StringBuilder(scan.nextLine());
        scan.close();

        Deque decA = new ArrayDeque();
        Deque decB = new ArrayDeque();

        int personNumber = 1;
        boolean aIsClose = false;
        boolean bIsClose = false;
        StringBuilder res = new StringBuilder();
        for (int i = 0; i < n; i++) {
            char ch = str.charAt(i);
            if (ch == 'a') {
                if (bIsClose) {
                    decB.addFirst(personNumber++);
                } else {
                    decA.addLast(personNumber++);
                }
            } else if (ch == 'b') {
                if (aIsClose) {
                    decA.addFirst(personNumber++);
                } else {
                    decB.addLast(personNumber++);
                }
            } else if (ch == 'A') {
                if (decA.size() > 0) {
                    res.append((int) decA.removeFirst() % 10);
                } else {
                    res.append((int) decB.removeLast() % 10);
                }
            } else if (ch == 'B') {
                if (decB.size() > 0) {
                    res.append((int) decB.removeFirst() % 10);
                } else {
                    res.append((int) decA.removeLast() % 10);
                }
            } else if (ch == '>') {
                aIsClose = true;
            } else if (ch == ']') {
                bIsClose = true;
            } else if (ch == '<') {
                aIsClose = false;
                while (decA.size() + 1 < decB.size()) {
                    decA.addLast(decB.removeLast());
                }
                if (decA.size() > decB.size()) {
                    while (decB.size() < decA.size()) {
                        decB.addLast(decA.removeLast());
                    }
                }
            } else if (ch == '[') {
                bIsClose = false;
                while (decB.size() + 1 < decA.size()) {
                    decB.addLast(decA.removeLast());
                }
                if (decB.size() > decA.size()) {
                    while (decA.size() < decB.size()) {
                        decA.addLast(decB.removeLast());
                    }
                }
            }
        }
        System.out.println(res);
    }

    public static void mainProgram3() throws IOException {
        MyScanner scan = new MyScanner(System.in);
        int n = scan.nextInt();
        scan.nextLine();


        Deque decA = new ArrayDeque();
        Deque decB = new ArrayDeque();

        int personNumber = 1;
        boolean aIsClose = false;
        boolean bIsClose = false;
        StringBuilder res = new StringBuilder();
        for (int i = 0; i < n; i++) {
            char ch = scan.nextChar();
            if (ch == 'a') {
                if (bIsClose) {
                    decB.addFirst(personNumber++);
                } else {
                    decA.addLast(personNumber++);
                }
            } else if (ch == 'b') {
                if (aIsClose) {
                    decA.addFirst(personNumber++);
                } else {
                    decB.addLast(personNumber++);
                }
            } else if (ch == 'A') {
                if (decA.size() > 0) {
                    res.append((int) decA.removeFirst() % 10);
                } else {
                    res.append((int) decB.removeLast() % 10);
                }
            } else if (ch == 'B') {
                if (decB.size() > 0) {
                    res.append((int) decB.removeFirst() % 10);
                } else {
                    res.append((int) decA.removeLast() % 10);
                }
            } else if (ch == '>') {
                aIsClose = true;
            } else if (ch == ']') {
                bIsClose = true;
            } else if (ch == '<') {
                aIsClose = false;
                while (decA.size() + 1 < decB.size()) {
                    decA.addLast(decB.removeLast());
                }
                if (decA.size() > decB.size()) {
                    while (decB.size() < decA.size()) {
                        decB.addLast(decA.removeLast());
                    }
                }
            } else if (ch == '[') {
                bIsClose = false;
                while (decB.size() + 1 < decA.size()) {
                    decB.addLast(decA.removeLast());
                }
                if (decB.size() > decA.size()) {
                    while (decA.size() < decB.size()) {
                        decA.addLast(decB.removeLast());
                    }
                }
            }
        }
        scan.close();
        System.out.println(res);
    }

    public static void mainProgram4() throws IOException {
        MyScanner scan = new MyScanner(System.in);
        int n = scan.nextInt();
        scan.nextLine();


        Deck decA = new Deck();
        Deck decB = new Deck();

        int personNumber = 1;
        StringBuilder res = new StringBuilder();
        for (int i = 0; i < n; i++) {
            char ch = scan.nextChar();
            if (ch == 'a') {
                decA.pushBack(personNumber++);
            } else if (ch == 'b') {
                decB.pushBack(personNumber++);
            } else if (ch == 'A') {
                res.append(decA.popFront() % 10);
            } else if (ch == 'B') {
                res.append(decB.popFront() % 10);
            } else if (ch == '>') {
                decB.pushBackDeck(decA);
            } else if (ch == ']') {
                decA.pushBackDeck(decB);
            } else if (ch == '<') {
                decB.popBackDeck(decA);
            } else if (ch == '[') {
                decA.popBackDeck(decB);
            }
            //System.out.println(res);
        }
        scan.close();
        System.out.println(res);
    }

}
class Deck {
    DeckElement left = null;
    DeckElement right = null;

    DeckElement middle = null;

    DeckElement middleLeft = null;
    DeckElement middleRight = null;

    int middleNum = 0;
    int size = 0;

    Deck() {

    }

    public void pushBack(int value) {
        if (size == 0) {
            DeckElement element = new DeckElement(value, null, null);
            left = element;
            right = element;
            size++;

            middle = element;
            middleNum = 1;
            return;
        }

        DeckElement element = new DeckElement(value, right, null);
        right.isNullSetThis(element);
        right = element;
        size++;
        if (size == 3) {
            middleRight = right;
            middleNum = 2;
        } else if (size == 2) {
            middle = right;
            middleLeft = left;
            middleNum = 2;
        } else if (size % 2 == 0) {
            DeckElement subRight = middleRight;
            middleRight = middleRight.getNotThis(middle);
            middleLeft = middle;
            middle = subRight;
            middleNum++;
        }
    }

    public void pushFront(int value) {
        if (size == 0) {
            DeckElement element = new DeckElement(value, null, null);
            left = element;
            right = element;
            size++;
            return;
        }

        DeckElement element = new DeckElement(value, null, left);
        left.isNullSetThis(element);
        left = element;
        size++;

        // доделать middle
    }

    public int popBack() {
        if (size == 0) {
            throw new NoSuchElementException("empty");
        } else if (size == 1) {
            int value = right.value;
            clear();
            return value;
        }

        int value = right.value;
        DeckElement element = right.getNotNull();
        element.thisToNull(right);
        right = element;
        size--;
        if (size == 1) {
            middle = right;
            middleNum = 1;
        } else if (size == 2) {
            middle = right;
            middleLeft = left;
            middleNum = 2;
        } else if (size == 3) {
            middleLeft = left;
            middleRight = right;
            middle = middleRight.getNotThis(null);
            middleNum = 2;
        } else if (size % 2 == 1) {
            DeckElement subElement = middleLeft;
            middleLeft = middleLeft.getNotThis(middle);
            middle = subElement;
            middleRight = middle;
            middleNum--;
        }
        return value;
    }

    public int popFront() {
        if (size == 0) {
            throw new NoSuchElementException("empty");
        } else if (size == 1) {
            int value = left.value;
            clear();
            return value;
        }

        int value = left.value;
        DeckElement element = left.getNotNull();
        element.thisToNull(left);
        left = element;
        size--;
        return value;
    }

    public void clear() {
        left = null;
        right = null;
        size = 0;
        middleNum = 0;
        middle = null;
        middleLeft = null;
        middleRight = null;
    }

    public void pushBackDeck(Deck deck) {
        if (size > 0 && deck.size > 0) {
            deck.right.isNullSetThis(right);
            right.isNullSetThis(deck.right);
            right = deck.left;
            size += deck.size;
            deck.clear();
            for (int i = middleNum; i <= size - size / 2 - size % 2 + 1; i++) {
                DeckElement subElement = middle;
                middle = middle.getNotThis(middle);
                middleLeft = subElement;
                middleNum++;
            }
            middleRight = middle.getNotThis(middleLeft);
        } else if (size == 0 && deck.size > 0) {
            left = deck.right;
            right = deck.left;
            size = deck.size;
            middle = deck.middle;
            middleRight = deck.middleLeft;
            middleLeft = deck.middleRight;
            if (size == 2) {
                middle = right;
                middleRight = null;
                middle = middleLeft;
                middleNum = 2;
            } else if (size % 2 == 0) {
                DeckElement subDeck = middle;
                middleRight = middleRight.getNotThis(middle);
                middle = middleRight;
                middleLeft = subDeck;
                middleNum = size - size / 2 - size % 2 + 1;
            }
            deck.clear();
        } else if (size > 0 && deck.size == 0) {
            return;
        }
    }

    public void popBackDeck(Deck deck) {
       // System.err.println(size + " " + deck.size);
        if (size == 1 || size == 0) {
            return;
        }

        int amount = size / 2;

        if (amount == 1) {
            deck.pushBack(popBack());
            return;
        }

        middle.thisToNull(middleLeft);
        middleLeft.thisToNull(middle);
        deck.left = right;
        deck.right = middle;
        deck.size = amount;


//        DeckElement lastElement = null;
//        DeckElement element = right;
//        for (int i = 1; i < amount; i++) {
//                DeckElement subElement = element;
//                element = element.getNotThis(lastElement);
//                lastElement = subElement;
//        }
//
//        DeckElement nextElement = element.getNotThis(lastElement);
//
//        lastElement = right;
//        element.thisToNull(nextElement);
//        nextElement.thisToNull(element);
//        deck.right = element;
//        deck.left = lastElement;
//        deck.size = amount;
//        right = nextElement;
//        size -= amount;
    }
}

class DeckElement {
    DeckElement leftElement;
    DeckElement rightElement;
    int value;

    DeckElement(int value, DeckElement leftElement, DeckElement rightElement) {
        this.leftElement = leftElement;
        this.rightElement = rightElement;
        this.value = value;
    }

    public DeckElement getNotNull() {
        if (leftElement == null) {
            return rightElement;
        } else {
            return leftElement;
        }
    }

    public void isNullSetThis(DeckElement element) {
        if (leftElement == null) {
            leftElement = element;
        } else {
            rightElement = element;
        }
    }

    public void thisToNull(DeckElement element) {
        if (leftElement == element) {
            leftElement =null;
        } else {
            rightElement = null;
        }
    }

    public DeckElement getNotThis(DeckElement element) {
        if (rightElement == element) {
            return leftElement;
        } else {
            return rightElement;
        }
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
