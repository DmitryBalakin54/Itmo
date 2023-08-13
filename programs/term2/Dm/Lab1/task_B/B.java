import java.io.*;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

public class B {
    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream("problem2.in")));

        String word = reader.readLine();

        String[] tmp = reader.readLine().trim().split(" +");

        int n = Integer.parseInt(tmp[0]);
        int m = Integer.parseInt(tmp[1]);
        int k = Integer.parseInt(tmp[2]);

        tmp = reader.readLine().trim().split(" +");
        Map<Integer, Integer> states = new HashMap<>();
        for (int i = 0; i < k; i++) {
            states.put(Integer.parseInt(tmp[i]), 1);
        }

        boolean[][][] way = new boolean[n + 1][n + 1][26];

        for (int i = 0; i < m; i++) {
            tmp = reader.readLine().trim().split(" +");
            way[Integer.parseInt(tmp[0])][Integer.parseInt(tmp[1])][tmp[2].charAt(0) - 'a'] = true;
        }

        reader.close();

        FileWriter writer = new FileWriter("problem2.out");
        HashSet<Integer> startPos = new HashSet<>();
        startPos.add(1);
        int res = search(word, 0, way, startPos, states);
        if (res == 0) {
            writer.write("Rejects");
        } else {
            writer.write("Accepts");
        }
        writer.close();
    }

    static int search(String word, int wordIndex, boolean[][][] way, HashSet<Integer> currPoses, Map<Integer, Integer> states) {
        if (wordIndex == word.length()) {
            for (int pos : currPoses) {
                if (states.containsKey(pos)) {
                    return pos;
                }
            }
            return 0;
        }
        HashSet<Integer> newPoses = new HashSet<>();
        char ch = word.charAt(wordIndex);
        for (int index : currPoses) {
            for (int i = 1; i < way.length; i++) {
                if (way[index][i][ch - 'a']){
                    newPoses.add(i);
                }
            }
        }
        return search(word, wordIndex + 1, way, newPoses, states);
    }
}
