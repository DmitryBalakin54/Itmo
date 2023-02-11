import java.util.*;

public class task5 {
    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);
        int n = scan.nextInt();
        int k = scan.nextInt();
        scan.close();
        firstMethod(n, k);
    }

    public static void firstMethod(int n, int k) {
        List<StringBuilder> list = new ArrayList<>();
        makeVectors(list, n, k);
        for (StringBuilder str : list) {
            System.out.println(str);
        }
    }

    public static void makeVectors(List<StringBuilder> list, int n, int k) {
        if (n == 1) {
            for (int i = 0; i < k; i++) {
                list.add(new StringBuilder());
                list.get(i).append(i);
            }
            return;
        }

        makeVectors(list, n - 1, k);
        int size = list.size();
        for (int i = 1; i < k; i++) {
            for (int j = 0; j < size; j++) {
                if (i % 2 == 1) {
                    list.add(new StringBuilder(list.get(size - 1 - j)).insert(0, i));
                } else {
                    list.add(new StringBuilder(list.get(j)).insert(0, i));
                }
            }
        }
        for (int i = 0; i < size; i++) {
            list.get(i).insert(0, 0);
        }
    }
}
