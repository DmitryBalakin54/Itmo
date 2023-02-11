import java.io.*;
import java.util.*;

public class F {
    public static void main(String[] args) throws IOException {
        mainProgram();
    }

    public static void mainProgram() throws IOException {
            MyScanner scan = new MyScanner(System.in);
            int n = scan.nextInt();
            int m = scan.nextInt();
            HeapStack heap = new HeapStack(n);
            for (int i = 0; i < m; i++) {
                scan.nextLine();
                int operation = scan.nextInt();
                if (operation == 1) {
                    int result = heap.getMax();
                    System.out.print(result);
                    if (result != -1) {
                        System.out.print(" ");
                        System.out.println(heap.lastMax);
                    } else {
                        System.out.println();
                    }
                } else if (operation == 2) {
                    int result = heap.add(scan.nextInt());
                    System.out.println(result);
                } else {
                    int result = heap.delete(scan.nextInt());
                    if (result == -1) {
                        System.out.println(result);
                    } else {
                        System.out.println(heap.lastDeleted);
                    }
                }
                //System.out.println(Arrays.toString(heap.buffer) + " " + heap.amount);
            }
            scan.close();
            System.out.println(heap);
        }

        static class HeapStack {
            private final int[] buffer;
            private int amount;

            public int lastDeleted;
        public  int lastMax;
        private int lastIndex;

        private  int maxIndex;

        HeapStack(int n) {
            buffer = new int[n + 1];
        }

        public int add(int element) {
            if (amount == buffer.length - 1) {
                return -1;
            }

            int index = ++amount;
            buffer[index] = element;
            while (index > 1) {
                int parentIndex = index / 2;
                if (buffer[index] > buffer[parentIndex]) {
                    swap(index, parentIndex);
                    index = parentIndex;
                } else {
                    break;
                }
            }
            return index;
        }

        public int siftDown(int index) {
            while (true) {
                int childLeftIndex = 2 * index;
                int childRightIndex = childLeftIndex + 1;
               // System.err.println(index + " " + childLeftIndex + "" + childRightIndex);
                if (childRightIndex <= amount) {
                    if (buffer[childLeftIndex] >= buffer[childRightIndex]) {
                        if (buffer[index] <= buffer[childLeftIndex]) {
                            swap(index, childLeftIndex);
                            index = childLeftIndex;
                        } else {
                            break;
                        }
                    } else {
                        if (buffer[index] <= buffer[childRightIndex]) {
                            swap(index, childRightIndex);
                            index = childRightIndex;
                        } else {
                            break;
                        }
                    }
                } else if (childLeftIndex <= amount) {
                    if (buffer[index] < buffer[childLeftIndex]) {
                        swap(index, childLeftIndex);
                        index = childLeftIndex;
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            }

            return index;
        }

        public int delete(int index) {
            if (amount < index || index < 1) {
                return -1;
            }
            if (index != amount) {
                swap(index, amount);
            }
            lastDeleted = buffer[amount--];
            lastIndex = siftDown(index);
            return 0;
        }

        public int getMax() {
            if (amount == 0) {
                return -1;
            } else if (amount == 1) {
                amount--;
                lastMax = buffer[1];
                return 0;
            }

            lastMax = buffer[1];
            maxIndex = siftDown(1);
            delete(maxIndex);
            return maxIndex;
        }
        public void swap(int index1, int index2) {
            if (index1 != index2) {
                buffer[index1] += buffer[index2];
                buffer[index2] = buffer[index1] - buffer[index2];
                buffer[index1] -= buffer[index2];
            }
//            int a = buffer[index1];
//            buffer[index1] = buffer[index2];
//            buffer[index2] = a;
        }

        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder();
            for (int i = 1; i <= amount; i++) {
                sb.append(buffer[i]).append(" ");
            }
            if (sb.length() > 0) {
                sb.delete(sb.length() - 1, sb.length() - 1);
            }
            return sb.toString();
        }

    }


    static class MyScanner {

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

}
