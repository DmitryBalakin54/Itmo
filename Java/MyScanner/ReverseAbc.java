import java.util.*;
import java.lang.*;
import java.io.*;
public class ReverseAbc {
	public static void main(String[] args) {
		ReverseScan();
	}
	
	public static void ReverseScan(){
		String[][] resultInts = new String[2][];
        MyScanner scan = null;
        int i = 0;
		try {
			scan = new MyScanner(System.in);
			while (true) {
				 if (resultInts.length - 1 <= i){
					resultInts = Arrays.copyOf(resultInts, resultInts.length * 2);
				}
				resultInts[i] = new String[2];
				int j = 0;
				while (true) {
					if (resultInts[i].length - 1 < j) {
						resultInts[i] = Arrays.copyOf(resultInts[i], resultInts[i].length * 2);
					}
					try {
						resultInts[i][j] = scan.next();
						j++;
					} catch (IOException e) {
						break;
					}
					
				} 
				
				
				//maxSize = Math.max(maxSize, j);
				if (resultInts[i].length  >= j) {
					resultInts[i] = Arrays.copyOf(resultInts[i], j);
				}
				
				i++;
				 try {
					scan.nextLine();
				} catch (IOException e) {
					break;
				} 
			} 
			
			try {
				scan.close();
			} catch (IOException e) {
				
			}
			
			if (resultInts.length  >= i) {
				resultInts = Arrays.copyOf(resultInts, i);
			}
		} catch (IOException e) {
			
		} finally {
			try {
				scan.close();
			} catch (IOException e) {
				
			}
		}
	    for (int j = resultInts.length - 1; j >= 0; j--) {
			for (int w = resultInts[j].length - 1; w >= 0; w-- ) {
			    System.out.print(resultInts[j][w] + " ");
			}
			System.out.println();
		}  
		
	}
	
	
}
