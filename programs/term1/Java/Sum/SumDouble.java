import java.lang.String;

public class SumDouble {
	public static void main( String[] args) {
		double sum = 0;
		for( String str : args ) {
			char[] charsFromStr = str.toCharArray();
			for ( int i = 0 ; i < charsFromStr.length ; i++ ) {
				if ( Character.isWhitespace( charsFromStr[i] )) {
					charsFromStr[i] = ' ';
				}
			}  
			str = String.valueOf(charsFromStr);
			if (str.length() != 0) {
			    String[] numbers = str.split(" ");
			    for (String num : numbers) {
					if (num.length() != 0) {
				        sum += Double.parseDouble(num);
					}
			    }
			}
		} 
        System.out.println(sum);		
	}   
}