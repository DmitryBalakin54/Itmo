import java.awt.image.AreaAveragingScaleFilter;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;

public class D2 {
    public static void main(String[] args) throws IOException {
        mainProgram();
    }

    static void mainProgram() throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        String[] s = reader.readLine().split(" ");
        int n = Integer.parseInt(s[0]);
        int q = Integer.parseInt(s[1]);
        char[] string = reader.readLine().toCharArray();
        for (int f = 0; f < q; f++) {
            s = reader.readLine().split(" ");
            int i = Integer.parseInt(s[0]);
            int j = Integer.parseInt(s[1]);
            int k = Integer.parseInt(s[2]);
            if (k == 1) {
                Arrays.sort(string, i - 1, j - 1);
            } else {
                char[] arr = Arrays.copyOfRange(string, i - 1, j);
                Arrays.sort(arr);
                System.err.println(string);
                System.err.println(arr);
                for (int r = 0; r < arr.length; r++) {
                    string[r + i - 1] = arr[arr.length - 1 - r];
                }
            }
        }
        for (char ch : string) {
            System.out.print(ch);
        }
        reader.close();
    }
}
