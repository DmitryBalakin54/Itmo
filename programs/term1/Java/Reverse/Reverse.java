import java.util.*;
import java.lang.*;
import java.io.*;
public class Reverse {
	public static void main(String[] args) {
       //Reverse1();    //5    3100+
	   //Reverse2();    //4    2800+
	   //Reverse3();    //3    2800+
	   //Reverse4();    //6    8900+ 
       //Reverse5();    //2    2800+
	   Reverse6();    //1    1800+
	   //Reverse7();    //фигня    3800+
       //Reverse8();	//1.5      2300+
       //Reverse6_1();	   
	}
	
	
	public static void Reverse6_1(){
		ArrayList<StringBuffer> resultStrings = new ArrayList<StringBuffer>();
        Scanner lineScan = new Scanner(System.in);
        int i = 0;
		int maxLen = 0;
        while (lineScan.hasNextLine()) {
            //StringBuffer string = new StringBuffer();
            String line = lineScan.nextLine();
            String[] numbers = line.split(" ");
			for(String s : numbers) {
				if(s.length() != 0) {
					if(resultStrings.get(i).length() == 0) {
					resultStrings.get(i).insert(0 ,s);
				} else {
				    resultStrings.get(i).insert(0 ,s + " ");
				}
				}
			}
            //resultStrings.add(string);
            i++;
		}
			     for(int j = i - 1; j >= 0; j-- ) {
            System.out.println(resultStrings.get(j));
        }
	}

	public static void Reverse1(){
		String[] resultStrings = new String[1000000];
        Scanner lineScan = new Scanner(System.in);
        int i = 0;
        while (lineScan.hasNextLine()) {
            String string = new String();
            String line = lineScan.nextLine();
            Scanner intScan = new Scanner(line);
            while(intScan.hasNextInt()) {
				if(string.length() == 0) {
					string += intScan.nextInt();
				} else {
				    string = intScan.nextInt() + " " + string;
				}
            }
            resultStrings[i] = string;
            i++;

        }

        for(int j = i - 1; j >= 0; j-- ) {
            System.out.println(resultStrings[j]);
        }
	}
	
	public static void Reverse2(){
		StringBuffer[] resultStrings = new StringBuffer[1000000];
        Scanner lineScan = new Scanner(System.in);
        int i = 0;
        while (lineScan.hasNextLine()) {
            StringBuffer string = new StringBuffer();
            String line = lineScan.nextLine();
            Scanner intScan = new Scanner(line);
            while(intScan.hasNextInt()) {
				if(string.length() == 0) {
					string.insert(0 ,intScan.nextInt());
				} else {
				    string.insert(0 ,intScan.nextInt() + " ");
				}
            }
            resultStrings[i] = string;
            i++;

        }

        for(int j = i - 1; j >= 0; j-- ) {
            System.out.println(resultStrings[j]);
        }
	}
	
	public static void Reverse3(){
		ArrayList<ArrayList<Integer>> resultInts = new ArrayList<ArrayList<Integer>>();
        Scanner lineScan = new Scanner(System.in);
        int i = 0;
        while (lineScan.hasNextLine()) {
            String line = lineScan.nextLine();
            Scanner intScan = new Scanner(line);
			resultInts.add(new ArrayList<Integer>());
			int j = 0;
            while(intScan.hasNextInt()) {
			    resultInts.get(i).add(intScan.nextInt());
				j++;
            }
            i++;

        }

        for(int w = i - 1; w >= 0; w-- ) {
			for(int j = resultInts.get(w).size() - 1; j >= 0; j--) {
				if( j == resultInts.get(w).size() - 1) {
					System.out.print(resultInts.get(w).get(j));
				} else {
					System.out.print(" " + resultInts.get(w).get(j));
				}
			}
			System.out.println();
        }
	}
	
	public static void Reverse4(){
		int[][] resultInts = new int[10000][10001];
        Scanner lineScan = new Scanner(System.in);
        int i = 0;
        while (lineScan.hasNextLine()) {
            String line = lineScan.nextLine();
            Scanner intScan = new Scanner(line);
			int j = 1;
            while(intScan.hasNextInt()) {
				resultInts[i][j] = intScan.nextInt();
				j++;
            }
            resultInts[i][0] = j;
            i++;

        }

         for(int w = i - 1; w >= 0; w-- ) {
			for(int j = resultInts[w][0] - 1; j >= 1; j--) {
				if( j == resultInts[w][0] - 1) {
					System.out.print(resultInts[w][j]);
				} else {
					System.out.print(" " + resultInts[w][j]);
				}
			}
			System.out.println();
        }
	}
	
	public static void Reverse5(){
		ArrayList<StringBuffer> resultStrings = new ArrayList<StringBuffer>();
        Scanner lineScan = new Scanner(System.in);
        int i = 0;
        while (lineScan.hasNextLine()) {
            StringBuffer string = new StringBuffer();
            String line = lineScan.nextLine();
            Scanner intScan = new Scanner(line);
            while(intScan.hasNextInt()) {
				if(string.length() == 0) {
					string.insert(0 ,intScan.nextInt());
				} else {
				    string.insert(0 ,intScan.nextInt() + " ");
				}
            }
            resultStrings.add(string);
            i++;

        }

        for(int j = i - 1; j >= 0; j-- ) {
            System.out.println(resultStrings.get(j));
        }
	}
	
	public static void Reverse6(){
		ArrayList<StringBuffer> resultStrings = new ArrayList<StringBuffer>();
        Scanner lineScan = new Scanner(System.in);
        int i = 0;
        while (lineScan.hasNextLine()) {
            StringBuffer string = new StringBuffer();
            String line = lineScan.nextLine();
            String[] numbers = line.split(" ");
			for(String s : numbers) {
				if(s.length() != 0) {
					if(string.length() == 0) {
					string.insert(0 ,s);
				} else {
				    string.insert(0 ,s + " ");
				}
				}
			}
            resultStrings.add(string);
            i++;

        }

       
		
		
		try {
			BufferedReader in = new BufferedReader(new InputStreamReader( new FileInputStream("out.txt")));
			ArrayList<String> t = new ArrayList<String>();
			while (true) {
				String s = in.readLine();
				if (s != null) {
				   t.add(s);
				} else {
					break;
				}
			}
			in.close();
			FileWriter out = new FileWriter("out.txt");
			for (int x = 0; x < t.size(); x++) {
				out.write(t.get(x).toString() + '\n');
			}
			for (int x = i -1; x >= 0; x--) {
				for (char c : resultStrings.get(x).toString().toCharArray()) {
					int h = c;
					out.write(h + "  ");
				}
				out.write('\n');
			}
			out.close();
		} catch (IOException e) {
			
		}
		for(int j = i - 1; j >= 0; j-- ) {
            System.out.println(resultStrings.get(j));
        }
	}
	
	public static void Reverse7(){
		int[][] resultInts = new int[0][];
        Scanner lineScan = new Scanner(System.in);
        int i = 0;
        while (lineScan.hasNextLine()) {
			resultInts = Arrays.copyOf(resultInts, resultInts.length + 1);
			resultInts[i] = new int[0];
            String line = lineScan.nextLine();
            Scanner intScan = new Scanner(line);
			int j = 0;
            while(intScan.hasNextInt()) {
				resultInts[i] = Arrays.copyOf(resultInts[i], resultInts[i].length + 1);
				resultInts[i][j] = intScan.nextInt();
				j++;
            }
            i++;
        }

         for(int w = i - 1; w >= 0; w-- ) {
			for(int j = resultInts[w].length - 1; j >= 0; j--) {
				if( j == resultInts[w].length - 1) {
					System.out.print(resultInts[w][j]);
				} else {
					System.out.print(" " + resultInts[w][j]);
				}
			}
			System.out.println();
        }
	}
	
	public static void Reverse8(){
		StringBuffer[] resultStrings = new StringBuffer[0];
        Scanner lineScan = new Scanner(System.in);
        int i = 0;
        while (lineScan.hasNextLine()) {
			resultStrings = Arrays.copyOf(resultStrings, resultStrings.length + 1);
            StringBuffer string = new StringBuffer();
            String line = lineScan.nextLine();
            String[] numbers = line.split(" ");
			for(String s : numbers) {
				if(s.length() != 0) {
					if(string.length() == 0) {
					string.insert(0 ,s);
				} else {
				    string.insert(0 ,s + " ");
				}
				}
			}
            resultStrings[i] = string;
            i++;

        }
		
		 for(int j = i - 1; j >= 0; j-- ) {
            System.out.println(resultStrings[j]);
        }
	}
}
