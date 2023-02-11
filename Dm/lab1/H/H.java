import java.util.Scanner;

public class H {
    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);
        int n = scan.nextInt();
        scan.close();
        StringBuilder result = new StringBuilder();
        result.append("((A0|B0)|(A0|B0))");
        for (int i = 1; i < n; i++) {
            result.insert(0, "((");
            result.append("|((A" + i + "|A" + i + ")|(B" + i + "|B" + i + ")))|(A" +
                i + "|B" + i + "))");
        }
        System.out.print(result);
    }
}
