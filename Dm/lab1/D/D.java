import java.io.*;
import java.util.*;

public class D {
    public static void main(String[] args) throws  IOException {
        mainProgram();
    }

    public static void mainProgram() throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        int n = Integer.parseInt(reader.readLine());

        List<StringBuilder> boxes = new ArrayList<>();
        for (int i = 0; i < n; i ++) {
            boxes.add(new StringBuilder("1 " + (i + 1)));
        }
        int number = 0;
        boolean flag = true;
        for (int i = 0; i < (int) Math.pow(2, n); i++) {
            boolean[] array = new boolean[n + 1];
            StringBuilder str = new StringBuilder(reader.readLine());
            for (int j = 0; j < n; j++) {
                array[j] = (str.charAt(j) - '0' == 1);
            }
            array[n] = (str.charAt(str.length() - 1)- '0' == 1);
           // System.err.println("arr is " + Arrays.toString(array));
            if (!array[n]) {
                flag = false;
                boxes.add(new StringBuilder("3 " + ((array[0]) ? n + 1 : 1) + " " +
                        ((array[1]) ? n + 2 : 2) ));
                for (int j = 2; j < n; j++) {
                    boxes.add( new StringBuilder("3 " + (boxes.size() + n) +
                            " " + (1 + j + ((array[j]) ? n : 0))));
                    //System.err.println(array[j] + " " + i + " " + j + " " + n);
                }
                if (number != 0) {
                    boxes.add(new StringBuilder("2 " + (boxes.size() + n) + " " + number));
                }
                number = boxes.size() + n;
            }
        }
        if (!flag) {
            System.out.println(n + boxes.size());
            for (StringBuilder str : boxes) {
                System.out.println(str);
            }
        }
        else {
            System.out.println(n + 2);
            System.out.println("1 1");
            System.out.println("3 1 " + (n + 1));
        }
    }
}

