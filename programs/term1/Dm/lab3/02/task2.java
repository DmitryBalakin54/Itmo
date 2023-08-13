import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class task2 {
    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);
        int n = scan.nextInt();
        scan.close();
        List<StringBuilder> list = grayCode(n);
        for (StringBuilder sb : list) {
            System.out.println(sb);
        }
    }

    public static List<StringBuilder> grayCode(int n) {
        if (n == 1) {
            return new ArrayList<>(List.of(new StringBuilder("0"), new StringBuilder("1")));
        }
        List<StringBuilder> list = grayCode(n - 1);
        int size = list.size();
        for (int i = size - 1; i > -1; i--) {
            list.add(new StringBuilder(list.get(i)));
            list.get(i).insert(0, "0");
            list.get(size + (size - i - 1)).insert(0, "1");
        }
        return list;
    }
}
