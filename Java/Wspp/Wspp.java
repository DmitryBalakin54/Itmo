import java.io.*;
import java.nio.charset.*;
import java.lang.Character.*;
import java.util.*;

public class Wspp {
	public static void main(String[] args) {
		wspp1(args[0], args[1]);
	}
	
	public static void wspp1(String fileIn, String fileOut) {
		try {
			MyScanner scan = new MyScanner(new FileInputStream(fileIn), "utf8");
			Map<String, IntList> words = new LinkedHashMap<>();
			String word = new String();
			int index = 0;
			while (true) {
				while (true) {
					try {
						word = scan.nextWord().toLowerCase();
						if (!words.containsKey(word)) {
							words.put(word, new IntList());
						}
						words.get(word).append(++index);
					} catch (IOException e) {
						break;
					}
				}
				try {
					scan.nextLine();
				} catch (IOException e) {
					break;
				}
				
			}
			
			try {
				FileWriter out = new FileWriter(fileOut, StandardCharsets.UTF_8);
				try {
					for(Map.Entry<String, IntList> w : words.entrySet()) {
						out.write(w.getKey() + " " + w.getValue() + "\n");
					}
				} catch(IOException e) {
					System.err.println(e.getMessage());
				} finally {
					out.close();
				}
			} catch (IOException e) {
				
			} finally {
				scan.close();
			}
		} catch (FileNotFoundException e) {
			System.err.println(e.getMessage());
		} catch (UnsupportedEncodingException e) {
			System.err.println(e.getMessage());
		} catch (IOException e) {
			System.err.println(e.getMessage());
		} 
	}
	
	
	public static void Wspp2(String fileIn, String fileOut) {
		try {
			MyScanner scan = new MyScanner(new FileInputStream(fileIn), "utf8");
			Map<String, int[]> words = new HashMap<>();
			String[] keys = new String[2];
			int ind = 0;
			String word = new String();
			int index = 0;
			while (true) {
				while (true) {
					try {
						word = scan.nextWord().toLowerCase();
						if (!words.containsKey(word)) {
							int[] nuls = {0,0};
							words.put(word, nuls);
							if (keys.length  == ind) {
								keys = Arrays.copyOf(keys, ind * 2);
							}
							keys[ind++] = word;
						}
						words.replace(word, append(words.get(word), ++index));
					} catch (IOException e) {
						break;
					}
				}
				try {
					scan.nextLine();
				} catch (IOException e) {
					break;
				}
				
			}
			
			try {
				FileWriter out = new FileWriter(fileOut, StandardCharsets.UTF_8);
				try {
					for(int i = 0; i < ind; i ++) {
						out.write(keys[i] + print(words.get(keys[i])) + "\n");
					}
				} catch(IOException e) {
					System.err.println(e.getMessage());
				} finally {
					out.close();
				}
			} catch (IOException e) {
				System.err.println(e.getMessage());
			} finally {
				scan.close();
			}
		} catch (FileNotFoundException e) {
			System.err.println(e.getMessage());
		}catch (UnsupportedEncodingException e) {
			System.err.println(e.getMessage());
		} catch (IOException e) {
			System.err.println(e.getMessage());
		}
	}
	
	public static int[] append(int [] array, int index) {
		//System.err.println(Arrays.toString(array));
		if (array.length == ++array[0]) {
			array = Arrays.copyOf(array, array[0]* 2);
		}
		array[array[0]] = index;
		return array;
	}
	
	public static String print(int[] ints) {
		StringBuilder str = new StringBuilder();
		for(int i = 0; i < ints[0] + 1; i++) {
			str.append(" " + ints[i]);
		}
		return str.toString();
	}
}



class IntList {
	
	private final int SIZE = 2; 
	private int amount;
	private int[] indexes;
	
	IntList() {
		amount = 0;
		indexes = new int[SIZE];
	}
	
	IntList(int amount, int[] indexes, int newIndex) {
		this.amount = amount;
		this.indexes = Arrays.copyOf(indexes, indexes.length);
		append(newIndex);
		
	}
	
	@Override
	public String toString() {
		StringBuilder str = new StringBuilder();
		str.append(amount);
	    for (int i = 0; i < amount; i++) {
            str.append(" " + indexes[i]);
		}			
		return str.toString();
	}
	
	public int getAmount() {
	    return amount;	
	}
	
	public int[] getIndexes() {
		return indexes;
	}
	
	public void append(int index) {
		indexes[amount] = index;
    	amount++;
		resize();
	}
	
	private void resize() {
		if (indexes.length == amount) {
			indexes = Arrays.copyOf(indexes, amount * 2);
		}
	}
	
}

