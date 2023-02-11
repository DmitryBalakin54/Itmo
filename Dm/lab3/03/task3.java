import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class task3 {
    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);
        int n = scan.nextInt();
        scan.close();
        //List<StringBuilder> list = ternAntiGrayCode(ternGrayCode(n));
        //List<StringBuilder> list = ternGrayCode(n);
        List<StringBuilder> list = ternAntiGrayCode(lexicographicallyCode(n));
        //List<StringBuilder> list = lexicographicallyCode(n);
        for (StringBuilder sb : list) {
            System.out.println(sb);
        }
    }

//    public static List<StringBuilder> ternGrayCode(int n){
//        if (n == 1) {
//            return new ArrayList<>(List.of(
//                    new StringBuilder("0"),
//                    new StringBuilder("1"),
//                    new StringBuilder("2")
//            ));
//        }
//        List<StringBuilder> list = ternGrayCode(n - 1);
//        int size = list.size();
//
//        for (int i = size - 1; i > -1; i--) {
//            list.add(new StringBuilder(list.get(i)));
//        }
//        for (int i = 0; i < size; i++) {
//            list.add(new StringBuilder(list.get(i)));
//        }
//
//        for (int j = 0; j < list.size(); j++) {
//           list.get(j).insert(0, j / size);
//        }
//        return list;
//    }

    public static List<StringBuilder>  lexicographicallyCode(int n) {
        List<StringBuilder> list = new ArrayList<>();
        StringBuilder  vector = new StringBuilder();
        for (int i = 0; i < n; i++) {
            vector.append('0');
        }
        for (int i = 0; i < Math.pow(3, n - 1); i++) {
            list.add(new StringBuilder(vector));
            for (int j = n - 1; j > -1; j--) {
                if (vector.charAt(j) == '1') {
                    vector.replace(j, j + 1, "2");
                    break;
                } else if (vector.charAt(j) == '2') {
                    vector.replace(j, j + 1, "0");
                } else {
                    vector.replace(j, j + 1, "1");
                    break;
                }
            }
        }
        return list;
    }

    public static List<StringBuilder> ternAntiGrayCode(List<StringBuilder> list) {
        int size = list.size();
        List<StringBuilder> antiList = new ArrayList<>();
        for (int i = 0; i < size; i++) {
            antiList.add(new StringBuilder(list.get(i)));
            StringBuilder sb1 = new StringBuilder();
            StringBuilder sb2 = new StringBuilder();
            for (int j = 0; j < list.get(i).length(); j++) {
                if (list.get(i).charAt(j) == '0') {
                    sb1.append('1');
                    sb2.append('2');
                } else if (list.get(i).charAt(j) == '1') {
                    sb1.append('2');
                    sb2.append('0');
                } else {
                    sb1.append('0');
                    sb2.append('1');
                }
            }
            antiList.add(sb1);
            antiList.add(sb2);
        }
        return antiList;
    }
}
