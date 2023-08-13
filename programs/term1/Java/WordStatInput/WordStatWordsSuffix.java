import java.util.*;
import java.io.*;
import java.nio.charset.*;
import java.lang.Character.*;

public class WordStatWordsSuffix {
    public static void main(String[] args) {
		WordStatInput(args[0], args[1]);   
	}
	
    public static int insertIntoArray(String[] arrayStr, int[] arrayInt , String element, int count) {
		if (element.length() > 3) {
			element = element.substring(element.length() - 3);
		}
		if(count == 0) {
			arrayStr[count] = element;
			arrayInt[count] = 1;
			count++;
		} else {
			boolean inString = false;
			for(int j = 0; j < count; j++) {
				if(element.equals(arrayStr[j])) {
					arrayStr[j] = element;
					arrayInt[j] += 1;
					inString = true;
					break;
				}
			}
			if(!inString) {
				arrayStr[count] = element;
				arrayInt[count] = 1;
				count++;
			}
		}
		return count;
	}
	
	public static void WordStatInput(String fileIn, String fileOut) {
		try {
			BufferedReader in = new BufferedReader(new InputStreamReader( new FileInputStream(fileIn), "utf8"));
			try {
				FileWriter out = new FileWriter(fileOut, StandardCharsets.UTF_8);
				String[] strings = new String[2];
				int[] amount = new int[2];
				int elements = 0;
				while (true) {
					String line = in.readLine();
					if(line == null) {
						break;
					} else if(line.length() == 0) {
						continue;
					}
					line = line.toLowerCase();
					int startSubstr = 0;
					String substr = new String();
					
					for (int i = 0; i <= line.length(); i++) {
						if (i != line.length()) { 
						    if (!(Character.isLetter(line.charAt(i)) ||  line.charAt(i) == '\'' || Character.getType(line.charAt(i)) == Character.DASH_PUNCTUATION)) {
								if (startSubstr - i < 0) {
									substr = line.substring(startSubstr, i);
						    		startSubstr = i + 1;
									elements = insertIntoArray(strings, amount, substr, elements);
									if(strings.length  == elements) {
										strings = Arrays.copyOf(strings, strings.length * 2);
										amount = Arrays.copyOf(amount, amount.length * 2);
									}
								} else {
									startSubstr++;
								}
							}
						} else if ((Character.isLetter(line.charAt(i - 1)) ||  line.charAt(i - 1) == '\'' || Character.getType(line.charAt(i - 1)) == Character.DASH_PUNCTUATION)) {
							if (startSubstr - i < 0) {
								substr = line.substring(startSubstr, i);
								elements = insertIntoArray(strings, amount, substr, elements);
								if(strings.length  == elements) {
									strings = Arrays.copyOf(strings, strings.length * 2);
									amount = Arrays.copyOf(amount, amount.length * 2);
								}
							}
						}							
					}	
				}
			    if(strings.length != elements) {
					strings = Arrays.copyOf(strings, elements);
					amount = Arrays.copyOf(amount, elements);
				}
				String swap = new String();
				for (int i = 0; i < strings.length - 1; i++) {
				    for (int j = 0; j < strings.length - 1; j++) {
                        if ( ( (strings[j].compareTo(strings[j + 1]) > 0)  )) {
							swap = strings[j];
							strings[j] = strings[j + 1];
							strings[j + 1] = swap;
						    amount[j] += amount[j + 1];
						    amount[j + 1] = amount[j] - amount[j + 1];
							amount[j] -= amount[j + 1];
						} 
					}						
				}
				try {	
					for( int i = 0; i < strings.length; i++) {
						out.write(strings[i] + " " + amount[i] + '\n');
					}
					
				} catch(IOException e) {
					System.out.println("Output or input error: " + e);
				} finally {
					out.close();
				}
			} catch(IOException e) {
					System.out.println("file " + fileOut + " cannot be opened" + e);
			} finally {
			     in.close();
			}
			
		} catch(FileNotFoundException e) {
			System.out.println("File " + fileIn + " is not found: " + e);
		} catch(UnsupportedEncodingException e) {
			System.out.println("unsupported encoding in file " + fileIn + ": " + e);
		} catch (IOException e) {
			
		}
	}
	
}




/* public static void WordStatInput(String fileIn, String fileOut) {
		BufferedReader in = null;
		try {
			in = new BufferedReader(new InputStreamReader( new FileInputStream(fileIn), "utf8"));
			FileWriter out = null;
			try {
				out = new FileWriter(fileOut, StandardCharsets.UTF_8);
				String[] strings = new String[2];
				int[] amount = new int[2];
				int elements = 0;
				while (true) {
					String line = in.readLine();
					if(line == null) {
						break;
					} else if(line.length() == 0) {
						continue;
					}
					line = line.toLowerCase();
					int startSubstr = 0;
					String substr = new String();
					
					for (int i = 0; i <= line.length(); i++) {
						if (i != line.length()) { 
						    if (!(Character.isLetter(line.charAt(i)) ||  line.charAt(i) == '\'' || Character.getType(line.charAt(i)) == Character.DASH_PUNCTUATION)) {
								if (startSubstr - i < 0) {
									substr = line.substring(startSubstr, i);
						    		startSubstr = i + 1;
									elements = insertIntoArray(strings, amount, substr, elements);
									if(strings.length  == elements) {
										strings = Arrays.copyOf(strings, strings.length * 2);
										amount = Arrays.copyOf(amount, amount.length * 2);
									}
								} else {
									startSubstr++;
								}
							}
						} else if ((Character.isLetter(line.charAt(i - 1)) ||  line.charAt(i - 1) == '\'' || Character.getType(line.charAt(i - 1)) == Character.DASH_PUNCTUATION)) {
							if (startSubstr - i < 0) {
								substr = line.substring(startSubstr, i);
								elements = insertIntoArray(strings, amount, substr, elements);
								if(strings.length  == elements) {
									strings = Arrays.copyOf(strings, strings.length * 2);
									amount = Arrays.copyOf(amount, amount.length * 2);
								}
							}
						}							
					}	
				}
			    if(strings.length != elements) {
					strings = Arrays.copyOf(strings, elements);
					amount = Arrays.copyOf(amount, elements);
				}
				String swap = new String();
				for (int i = 0; i < strings.length - 1; i++) {
				    for (int j = 0; j < strings.length - 1; j++) {
                        if ( ( (strings[j].compareTo(strings[j + 1]) > 0)  )) {
							swap = strings[j];
							strings[j] = strings[j + 1];
							strings[j + 1] = swap;
						    amount[j] += amount[j + 1];
						    amount[j + 1] = amount[j] - amount[j + 1];
							amount[j] -= amount[j + 1];
						} 
					}						
				}
				try {	
					for( int i = 0; i < strings.length; i++) {
						out.write(strings[i] + " " + amount[i] + '\n');
					}
					
				} catch(IOException e) {
					System.out.println("Output or input error: " + e);
				}
			} catch(IOException e) {
					System.out.println("file " + fileOut + " cannot be opened" + e);
			} finally {
				try {
				   out.close();
				} catch (IOException e) {
					
				}
			}
			
		} catch(FileNotFoundException e) {
			System.out.println("File " + fileIn + " is not found: " + e);
		} catch(UnsupportedEncodingException e) {
			System.out.println("unsupported encoding in file " + fileIn + ": " + e);
		} finally {
			try {
			    in.close();
			} catch (IOException e) {
				
			}
		}
	} */
