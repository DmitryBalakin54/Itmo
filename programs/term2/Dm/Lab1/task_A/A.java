import java.io.*;
import java.util.HashMap;
import java.util.Map;

public class A {
    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream("problem1.in")));

        String word = reader.readLine();

        String[] tmp = reader.readLine().split(" ");

        int n = Integer.parseInt(tmp[0]);
        int m = Integer.parseInt(tmp[1]);
        int k = Integer.parseInt(tmp[2]);

        tmp = reader.readLine().split(" ");
        Map<Integer, Integer> states = new HashMap<>();
        for (int i = 0; i < k; i++) {
            states.put(Integer.parseInt(tmp[i]) - 1, 1);
        }

        int[][] way = new int[n][26];
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < 26; j++) {
                way[i][j] = -1;
            }
        }
        for (int i = 0; i < m; i++) {
            tmp = reader.readLine().split(" ");
            way[Integer.parseInt(tmp[0]) - 1][tmp[2].charAt(0) - 'a'] = Integer.parseInt(tmp[1]) - 1;
        }

        reader.close();

        FileWriter writer = new FileWriter("problem1.out");

        boolean res = true;
        int index = 0;
        for (int i = 0; i < word.length(); i++) {
            index = way[index][word.charAt(i) - 'a'];
            if (index == -1) {
                res = false;
                break;
            }
        }
        if (!res || !states.containsKey(index)) {
            writer.write("Rejects");
        } else {
            writer.write("Accepts");
        }
        writer.close();
    }
}
