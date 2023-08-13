import java.util.*;
import java.lang.*;
import java.io.*;
public class Reverse {
	public static void main(String[] args) {
		ReverseScan();
	}
	
	public static void ReverseScan(){
		int[][] resultInts = new int[2][]; 
		int i = 0;
		MyScanner scan = null; 
		try {
			scan = new MyScanner(System.in);
			while (true) {
				 if (resultInts.length - 1 <= i){
					resultInts = Arrays.copyOf(resultInts, resultInts.length * 2);
				}
				resultInts[i] = new int[2];
				int j = 0;
				while (true) {
					if (resultInts[i].length - 1 < j) {
						resultInts[i] = Arrays.copyOf(resultInts[i], resultInts[i].length * 2);
					}
					try {
						resultInts[i][j] = scan.nextInt();
						j++;
					} catch (IOException e) {
						break;
					}
					
				} 
				
				
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
