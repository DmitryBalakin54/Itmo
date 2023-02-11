import java.util.*;

public class C {
    public static void main(String[] args) {
        mainProgram();
    }

    public  static  void mainProgram() {
        Scanner scan = new Scanner(System.in);
        String str = scan.next();
        scan.close();
        List<StringBuilder> list = new ArrayList<>();
        for (int i = 0; i < str.length(); i++) {
            list.add(new StringBuilder());
        }
        for (int i = 0; i < str.length(); i++) {
            append(list, str);
            list.sort(null);
        }
        System.out.print(list.get(0));

    }

    public static void append(List<StringBuilder> list, String str) {
        for (int j = 0; j < list.size(); j++) {
            list.get(j).insert(0, str.charAt(j));
        }
    }
}

