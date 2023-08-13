import java.util.Scanner;

public class task {
    public static void main(String[] args) {
        mainProgram();
    }

    public static void mainProgram(){
        Scanner scan = new Scanner(System.in);
        int n = scan.nextInt();
        scan.close();
        StringBuilder vector = new StringBuilder();
        for (int i = 0; i < n; i++) {
            vector.append('0');
        }
        for (int j = 0; j < 1 << n; j++) {
            System.out.println(vector);
            for (int i = n - 1; i >= 0; i--) {
                if (vector.charAt(i) == '1') {
                    vector.replace(i, i + 1, "0");
                } else {
                    vector.replace(i, i + 1, "1");
                    break;
                }
            }
        }
    }
}
