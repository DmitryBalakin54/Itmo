import java.io.*;
import java.util.ArrayList;
import java.util.Scanner;

public class B7 {
    public static void main(String[] args) throws IOException {
        mainProgram();
    }

    public static void mainProgram() throws IOException {
        Scanner scan = new Scanner(System.in);
        int n = scan.nextInt();
        int m = scan.nextInt();
        int k = scan.nextInt();

        boolean[][] graph = new boolean[n + 1][n + 1];
        for (int i = 1; i <= n; i++) {
            graph[i][i] = true;
        }
        for (int i = 0; i < m; i++) {
            int vertex1 = scan.nextInt();
            int vertex2 = scan.nextInt();
            graph[vertex1][vertex2] = true;
            graph[vertex2][vertex1] = true;
        }

        for (int i = 0; i < k; i++) {
            String str = scan.next();
            int vertex1 = scan.nextInt();
            int vertex2 = scan.nextInt();

            if (str.equals("cut")) {
                cut(graph, vertex1, vertex2);
            } else if (str.equals("ask")) {
                if (ask(graph, vertex1, vertex2)) {
                    System.out.println("YES");
                } else {
                    System.out.println("NO");
                }
            }
        }
    }

}

class SNM {
    int[] parent;
    int[] rank;

    SNM(int n){
        parent = new int[n + 1];
        rank = new int[n + 1];

        for (int i = 1; i <= n; i++) {
            parent[i] = i;
        }
    }

    public int findParent(int a) {
        while (parent[a] != a) {
            a = parent[a];
        }
        return a;
    }

    public void join(int a, int b) {
        a = findParent(a);
        b = findParent(b);
        if (a != b) {
            if (rank[a] < rank[b]) {
                int c = a;
                a = b;
                b = c;
            }

            parent[b] = a;
            rank[a] = Math.max(rank[a], rank[b] + 1);
        }
    }

   

    public int get(int a) {
        if (parent[a] == a) {
            return value[a];
        } else {
            return value[a] + get(parent[a]);
        }
    }
}