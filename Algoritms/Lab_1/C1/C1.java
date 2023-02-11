import java.io.*;
import java.lang.Character.*;
import java.util.*;
import java.lang.*;

public class C1 {
	public static void main(String[] args) {
		try {
			MyScanner scan = new MyScanner(System.in);
			try {
			    int n = scan.nextInt();
				int[] array = buildBadArray(n);
				
				
				for (int i = 0; i < n; i++) {
					System.out.print(array[i] + " ");
				}
				
				
			} catch (IOException e) {
				System.err.println(e.getMessage());
			} finally {
				scan.close();
			}
		} catch (IOException e) {
			System.err.println(e.getMessage());
		}
	}

	public static int[] buildArray(int n) {
	    int[] array = new int[n];
		for (int i = 0; i < n; i++) {
			array[i] = i + 1;
		}
		return array;
	}


	public static int[] buildBadArray(int n) {
		int[] array = buildArray(n);
		for (int i=2;i<n;i++) {
			swap(array, i, i / 2);
		}
		return array;
	}

	private static void swap(int[] a, int i, int i1) {
		if (i != i1) {
			a[i] += a[i1];
			a[i1] = a[i] - a[i1];
			a[i] -= a[i1];
		}
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