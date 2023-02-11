import java.io.*;
import java.nio.charset.*;
import java.lang.Character.*;
import java.util.*;

public class WsppCountPosition {
	public static void main(String[] args) {
		wspp2(args[0], args[1]);
	}
	public static void wspp2(String fileIn, String fileOut) {
		try {
			MyScanner scan = new MyScanner(new FileInputStream(fileIn), "utf8");
			Comparator<IntListCP> comparator = new Comparator<>() {
				@Override
				public int compare(IntListCP o1, IntListCP o2) {
					int result = 0;
					if (o1.getAmount() > o2.getAmount()) {
						result = 1;
					} else if (o1.getAmount() == o2.getAmount()) {
						if (o1.getIndexes()[0] > o2.getIndexes()[0]) {
							result = 1;
						} else if (o1.getIndexes()[0] == o2.getIndexes()[0]) {
							if (o1.getIndexes()[1] > o2.getIndexes()[1]) {
								result = 1;
							} else {
								result = -1;
							}
						} else if (o1.getIndexes()[0] < o2.getIndexes()[0]) {
							result = -1;
						}
					} else if (o1.getAmount() < o2.getAmount()) {
						result = -1;
					}
					return result;
				}
			};
			Map<String, IntListCP> words = new HashMap<>();
			String word = new String();
			int index = 0;
			int str = 1;
			while (true) {
				while (true) {
					try {
						word = scan.nextWord().toLowerCase();
						if (!words.containsKey(word)) {
							words.put(word, new IntListCP());
						}
						words.get(word).appendStr(str);
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
				str++;
				index = 0;
			}

			Map<IntListCP, String> list = new TreeMap<>(comparator);
			for (Map.Entry<String, IntListCP> entry : words.entrySet()) {
				list.put(entry.getValue(), entry.getKey());
			}

			try {
				FileWriter out = new FileWriter(fileOut, StandardCharsets.UTF_8);
				try {
					for(Map.Entry<IntListCP, String> entry : list.entrySet()) {
						out.write(entry.getValue() + " " + entry.getKey() + "\n");
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

	public static void wspp(String fileIn, String fileOut) {
		try {
			MyScanner scan = new MyScanner(new FileInputStream(fileIn), "utf8");
			Map<String, IntListCP> words = new HashMap<>();
			String[] keys = new String[2];
			int ind = 0;
			String word = new String();
			int index = 0;
			int str = 1;
			while (true) {
				while (true) {
					try {
						word = scan.nextWord().toLowerCase();
						if (!words.containsKey(word)) {
							words.put(word, new IntListCP());
							if (keys.length  == ind) {
								keys = Arrays.copyOf(keys, ind * 2);
							} 
							keys[ind++] = word;
						}
						words.get(word).appendStr(str);
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
				str++; 
				index = 0;
			}

			BubbleSort(keys, ind, words);
			
			try {
				FileWriter out = new FileWriter(fileOut, StandardCharsets.UTF_8);
				try {
					for(int i = 0; i < ind; i ++) {
						out.write(keys[i] + " " + words.get(keys[i]) + "\n");
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
	
	public static void BubbleSort(String[] keys, int ind,  Map<String, IntListCP> words) {
		for (int i = 0; i < ind - 1; i++) {
				for (int j = 0; j < ind - 1; j++) {
				    if (words.get(keys[j]).getAmount() > words.get(keys[j + 1]).getAmount()) {
						swap(keys, j, j + 1);
					} else if (words.get(keys[j]).getAmount() == words.get(keys[j + 1]).getAmount()) {
						if (words.get(keys[j]).getIndexes()[0] > words.get(keys[j + 1]).getIndexes()[0]) {
							swap(keys, j, j + 1);
						} else if (words.get(keys[j]).getIndexes()[0] == words.get(keys[j + 1]).getIndexes()[0]) {
							if (words.get(keys[j]).getIndexes()[1] > words.get(keys[j + 1]).getIndexes()[1]) {
								swap(keys, j, j + 1);
					    	}
						}
					}
			    }
			}
	}
	
	public static void swap(String[] str, int l, int r) {
		String s = str[l];
		str[l] = str[r];
		str[r] = s;
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



class IntListCP {
	
	private final int SIZE = 2; 
	private int amount;
	private int[] indexes;
	
	IntListCP() {
		amount = 0;
		indexes = new int[SIZE];
	}
	
	IntListCP(int amount, int[] indexes, int newLine, int newIndex) {
		this.amount = amount;
		this.indexes = indexes;
		appendStr(newLine);
		append(newIndex);
		
	}
	
	@Override
	public String toString() {
		StringBuilder str = new StringBuilder();
		str.append(amount);
	    for (int i = 0; i < amount; i++) {
            str.append(" " + indexes[2 * i] + ":" + indexes[2 * i + 1]);
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
		indexes[ 2 * amount + 1] = index;
    	amount++;
		resize();
	}
	
	public void appendStr(int index) {
		indexes[2 * amount] = index;
		resize();
	}
	
	private void resize() {
		if (indexes.length <= 2 * amount + 1) {
			indexes = Arrays.copyOf(indexes, (2 * amount + 1) * 2);
		}
	}
	
}

