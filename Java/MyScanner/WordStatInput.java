import java.util.*;
import java.io.*;
import java.nio.charset.*;
import java.lang.Character.*;

public class WordStatInput {
    public static void main(String[] args) {
		//WordStatInput1(args[0], args[1]);   //2500+
		//WordStatInput2(args[0], args[1]);   //2600+
		WordStatInput3(args[0], args[1]);   //960+
	}
	
	
	public static void WordStatInput3(String fileIn, String fileOut) {
		
		/* try {
			BufferedReader in = new BufferedReader(new InputStreamReader( new FileInputStream(fileIn), "utf8"));
			while (true) {
				String line = in.readLine();
				if (line == null) {
					break;
				}
				System.err.println(line);
			}
			in.close();
			System.err.println();
		} catch (FileNotFoundException e) {
			
		} catch (UnsupportedEncodingException e) {
			
		} catch (IOException e) {
			
		} */
		try {
			MyScanner scan = new MyScanner(new FileInputStream(fileIn), "utf8");
			try {
				FileWriter out = new FileWriter(fileOut, StandardCharsets.UTF_8);
				String[] strings = new String[2];
				int[] amount = new int[2];
				int elements = 0;
				while(true) { 
					while (true) {
						try {
							String s =  scan.nextWord().toLowerCase();
							if(s.length() != 0) {
								if(elements == 0) {
									strings[elements] = s;
									amount[elements] = 1;
									elements++;
								} else {
									boolean inString = false;
									for(int i = 0; i < elements; i++) {
										if(s.equals(strings[i])) {
											strings[i] = s;
											amount[i] += 1;
											inString = true;
											break;
										}
									}
									if(!inString) {
										strings[elements] = s;
										amount[elements] = 1;
										elements++;
									}
								}
								if(strings.length  == elements) {
									strings = Arrays.copyOf(strings, strings.length * 2);
									amount = Arrays.copyOf(amount, amount.length * 2);
								}
							}	
						} catch (IOException e) {
							break;
						}
					}
					//System.err.println(Arrays.toString(strings));
					//System.err.println(Arrays.toString(amount));
					try {
						scan.nextLine();
					} catch (IOException e) {
						break;
					}
				}
				
			    if(strings.length != elements) {
					strings = Arrays.copyOf(strings, elements);
					amount = Arrays.copyOf(amount, elements);
				}
				
				try {	
					for( int i = 0; i < strings.length; i++) {
						out.write(strings[i] + " " + amount[i] + '\n');
					}
					try {
						out.close();
					} catch(IOException e) {
						System.out.println("Output or input error: " + e);
					}
				} catch(IOException e) {
					System.out.println("Output or input error: " + e);
				}
			} catch(IOException e) {
					System.out.println("file " + fileOut + " cannot be opened" + e);
			}
			try {
			    scan.close();
			} catch (IOException e) {
				System.out.println("Output or input error: " + e);
			}
		} catch(FileNotFoundException e) {
			System.out.println("File " + fileIn + " is not found: " + e);
		} catch(UnsupportedEncodingException e) {
			System.out.println("unsupported encoding in file " + fileIn + ": " + e);
		} catch (IOException e) {

		}
	}	




	
	public static void WordStatInput1(String fileIn, String fileOut) {
		try {
			BufferedReader in = new BufferedReader(new InputStreamReader( new FileInputStream(fileIn), "utf8"));
			try {
				FileWriter out = new FileWriter(fileOut, StandardCharsets.UTF_8);
				String[] strings = new String[2];
				int elements = 0;
				while(true) {
					String line = in.readLine();
					if(line == null) {
						break;
					} else if(line.length() == 0) {
						continue;
					}
					char[] chars = line.toLowerCase().toCharArray(); 
					for(int i = 0; i < chars.length; i++) {
						if( !(Character.isLetter(chars[i]) ||  chars[i] == '\''
						      || chars[i] == '\u002D' || chars[i] == '\u058A' || chars[i] == '\u05BE'
						      || chars[i] == '\u1400' || chars[i] == '\u1806' || chars[i] == '\u2010'
							  || chars[i] == '\u2011' || chars[i] == '\u2012' || chars[i] == '\u2013'
							  || chars[i] == '\u2014' || chars[i] == '\u2015' || chars[i] == '\u2E17'
							  || chars[i] == '\u2E1A' || chars[i] == '\u2E3A' || chars[i] == '\u2E3B'
							  || chars[i] == '\u2E40' || chars[i] == '\u301C' || chars[i] == '\u3030'
							  || chars[i] == '\u30A0' || chars[i] == '\uFE31' || chars[i] == '\uFE32'
							  || chars[i] == '\uFE58' || chars[i] == '\uFE63' || chars[i] == '\uFF0D'
							  /*|| chars[i] == '\u10EAD'*/)  ){
							chars[i] = ' ';
						} 
					}
					for(String s : String.valueOf(chars).split(" ")) {
						if(s.length() != 0) {
							if(elements == 0) {
								strings[elements] = s + " " + 1;
								elements++;
							} else {
								boolean inString = false;
								for(int i = 0; i < elements; i++) {
									if(s.equals(strings[i].split(" ")[0])) {
										strings[i] = s + " " + (Integer.parseInt(strings[i].split(" ")[1]) + 1);
										inString = true;
								        break;
									}
								}
								if(!inString) {
									strings[elements] = s + " " + 1;
									elements++;
								}
							}
						}
						if(strings.length  == elements) {
							strings = Arrays.copyOf(strings, strings.length * 2);
						}
					}
				}
			    if(strings.length != elements) {
					strings = Arrays.copyOf(strings, elements);
				}
				try {	
					for( String s : strings) {
						out.write(s + '\n');
					}
					try {
						out.close();
					} catch(IOException e) {
						
					}
				} catch(IOException e) {
					
				}
			} catch(IOException e) {
					
			}
			try {
			    in.close();
			} catch (IOException e) {
				
			}
		} catch(FileNotFoundException e) {
			System.out.println("File " + fileIn + " is not found: " + e);
		} catch(UnsupportedEncodingException e) {
			System.out.println("unsupported encoding in file " + fileIn + ": " + e);
		}
	}	
	
		public static void WordStatInput2(String fileIn, String fileOut) {
		try {
			BufferedReader in = new BufferedReader(new InputStreamReader( new FileInputStream(fileIn), "utf8"));
			try {
				FileWriter out = new FileWriter(fileOut, StandardCharsets.UTF_8);
				ArrayList<String> strings = new ArrayList<String>();
				int elements = 0;
				while(true) {
					String line = in.readLine();
					if(line == null) {
						break;
					} else if(line.length() == 0) {
						continue;
					}
					char[] chars = line.toLowerCase().toCharArray(); 
					for(int i = 0; i < chars.length; i++) {
						if( !(Character.isLetter(chars[i]) ||  chars[i] == '\''
						      || chars[i] == '\u002D' || chars[i] == '\u058A' || chars[i] == '\u05BE'
						      || chars[i] == '\u1400' || chars[i] == '\u1806' || chars[i] == '\u2010'
							  || chars[i] == '\u2011' || chars[i] == '\u2012' || chars[i] == '\u2013'
							  || chars[i] == '\u2014' || chars[i] == '\u2015' || chars[i] == '\u2E17'
							  || chars[i] == '\u2E1A' || chars[i] == '\u2E3A' || chars[i] == '\u2E3B'
							  || chars[i] == '\u2E40' || chars[i] == '\u301C' || chars[i] == '\u3030'
							  || chars[i] == '\u30A0' || chars[i] == '\uFE31' || chars[i] == '\uFE32'
							  || chars[i] == '\uFE58' || chars[i] == '\uFE63' || chars[i] == '\uFF0D'
							  /*|| chars[i] == '\u10EAD'*/)  ){
							chars[i] = ' ';
						} 
					}
					for(String s : String.valueOf(chars).split(" ")) {
						if(s.length() != 0) {
							if(elements == 0) {
								strings.add(elements, s + " " + 1);
								elements++;
							} else {
								boolean inString = false;
								for(int i = 0; i < elements; i++) {
									if(s.equals(strings.get(i).split(" ")[0])) {
										strings.set(i, s + " " + (Integer.parseInt(strings.get(i).split(" ")[1]) + 1));
										inString = true;
								        break;
									}
								}
								if(!inString) {
									strings.add(elements, s + " " + 1);
									elements++;
								}
							}
						}
					}
				}
				try {	
					for( String s : strings) {
						out.write(s + '\n');
					}
					try {
						out.close();
					} catch(IOException e) {
						
					}
				} catch(IOException e) {
					
				}
			} catch(IOException e) {
					
			}
			try {
			    in.close();
			} catch (IOException e) {
				
			}
		} catch(FileNotFoundException e) {
			System.out.println("File " + fileIn + " is not found: " + e);
		} catch(UnsupportedEncodingException e) {
			System.out.println("unsupported encoding in file " + fileIn + ": " + e);
		}
	}
	
	

}

/*try{
	BufferedReader in1 = new BufferedReader(new InputStreamReader( new FileInputStream(fileIn), "utf8"));
	try {
		FileWriter out1 = new FileWriter("debugging.txt", StandardCharsets.UTF_8);
		while(true) {
			String line1 = in1.readLine();
			if(line1 == null) {
				break;
			}
			out1.write(line1 + '\n');
		}
		try {
			out1.close();
		} catch(IOException e) {
		
		}
	} catch(IOException e) {
		
	}
	try {
		in1.close();
	} catch (IOException e) {

	}
} catch(FileNotFoundException e) {

} catch(UnsupportedEncodingException e) {

} */
					
