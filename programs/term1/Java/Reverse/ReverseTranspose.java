import java.util.*;
import java.lang.*;

public class ReverseTranspose {
	public static void main(String[] args) {
	   Reverse7();       
	}
	
	public static void Reverse7() {
		int[][] resultInts = new int[2][2];
        Scanner lineScan = new Scanner(System.in);
        int i = 0;
		int maxSize = 0;
        while (lineScan.hasNextLine()) {
			if (resultInts.length - 1 <= i) {
			    resultInts = Arrays.copyOf(resultInts, resultInts.length * 2);
			}
			resultInts[i] = new int[2];
            String line = lineScan.nextLine();
			
			Scanner intScan = new Scanner(line);
			int j = 0;
			while (intScan.hasNextInt()) {
				if (resultInts[i].length - 1 < j) {
			        resultInts[i] = Arrays.copyOf(resultInts[i], resultInts[i].length * 2);
				}
				resultInts[i][j] = intScan.nextInt();;
				j++;
			}
			
			/*String[] numbers = line.split(" ");
			for (String s : numbers) {
				if (s.length() != 0) {
					if (resultInts[i].length - 10 <= j) {
				        resultInts[i] = Arrays.copyOf(resultInts[i], resultInts[i].length * 2);
					}
				    resultInts[i][j] = Integer.valueOf(s);
				    j++;
				}
			}*/
			
			maxSize = Math.max(maxSize, j);
			if (resultInts[i].length  >= j) {
				resultInts[i] = Arrays.copyOf(resultInts[i], j);
			}
			i++;
        } 
		if (resultInts.length  >= i){
		    resultInts = Arrays.copyOf(resultInts, i);
		}
		
		boolean space;
	    for (int j = 0; j <= maxSize - 1; j++) {
			space = false;
			for (int w = 0; w <= i - 1; w++ ) {
			    if ((w == 0) || !space) {
				    if (resultInts[w].length - 1 >= j) {
				        System.out.print(resultInts[w][j]);
						space = true;
				    }
			    } else {
					if (resultInts[w].length - 1 >= j) {
				        System.out.print(" " + resultInts[w][j]);
					}
			    }
			}
			System.out.println();
		}
	}
	
	
	public static void Reverse3(){
		ArrayList<ArrayList<Integer>> resultInts = new ArrayList<ArrayList<Integer>>();
        Scanner lineScan = new Scanner(System.in);
        int i = 0;
		int maxSize = 0;
        while (lineScan.hasNextLine()) {
            String line = lineScan.nextLine();
            Scanner intScan = new Scanner(line);
			resultInts.add(new ArrayList<Integer>());
			int j = 0;
            while(intScan.hasNextInt()) {
			    resultInts.get(i).add(intScan.nextInt());
				j++;
            }
			maxSize = Math.max(maxSize, j);
            i++;
        }
		boolean space;
	    for(int j = 0; j <= maxSize - 1; j++) {
			space = false;
			for(int w = 0; w <= i - 1; w++ ) {
			    if((w == 0) || (space == false)) {
				    if(resultInts.get(w).size() >= j + 1 ) {
				        System.out.print(resultInts.get(w).get(j));
						space = true;
				    }
			    } else {
					if(resultInts.get(w).size() >= j + 1) {
				        System.out.print(" " + resultInts.get(w).get(j));
					}
			    }
			}
			System.out.println();
		}
        
	}
	
}