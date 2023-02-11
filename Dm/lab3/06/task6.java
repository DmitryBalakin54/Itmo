import java.util.*;

public class task6 {
    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);
        int n = scan.nextInt();
        scan.close();
        List<StringBuilder> list = new ArrayList<>();
        makeVectors(n, list);
        System.out.println(list.size());
        for (StringBuilder str : list) {
            System.out.println(str);
        }
    }

    public static void makeVectors(int n, List<StringBuilder> list) {
        if (n == 1) {
            list.add(new StringBuilder("0"));
            list.add(new StringBuilder("1"));
            return;
        }

        makeVectors(n - 1, list);
        int size = list.size();
        for (int i = 0; i < size; i++) {
            if (list.get(i).charAt(0) == '0') {
                list.add(new StringBuilder(list.get(i)).insert(0, '1'));
            }
            list.get(i).insert(0, '0');
        }
    }
}