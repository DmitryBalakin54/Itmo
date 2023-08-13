import java.lang.String;

public class Sum {
	public static void main( String[] args) {
		int sum = 0;
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
				        sum += Integer.parseInt(num);
					}
			    }
			}
		} 
        System.out.println(sum);		
	}      
}