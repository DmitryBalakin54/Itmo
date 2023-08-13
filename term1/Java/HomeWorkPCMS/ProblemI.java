import java.util.Scanner;
import java.lang.Math;

public class ProblemI {
    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);
        int amount = scan.nextInt();
        int[][] values = new int[amount][3];
        int xLeft = 0;
        int xRight = 0;
        int yLeft = 0;
        int yRight = 0;
        for (int j = 0; j < amount; j++) {
            for (int i = 0; i < 3; i++) {
                values[j][i] = scan.nextInt();
            }
            if (j ==0) {
                xLeft = values[j][0] - values[j][2];
                xRight = values[j][0] + values[j][2];
                yLeft = values[j][1] - values[j][2];
                yRight = values[j][1] + values[j][2];
            } else {
                xLeft = Math.min(xLeft, values[j][0] - values[j][2]);
                xRight = Math.max(xRight, values[j][0] + values[j][2]);
                yLeft = Math.min(yLeft, values[j][1] - values[j][2]);
                yRight = Math.max(yRight, values[j][1] + values[j][2]);
            }
        }
        scan.close();
        System.out.println((xLeft + xRight) / 2 + " " + (yLeft + yRight) / 2 + " " +
            (int) Math.ceil( Math.max(xRight - xLeft, yRight - yLeft) / 2.0));
    }
}
