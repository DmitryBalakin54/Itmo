import java.util.Scanner;

public class A7 {
    public static void main(String[] args) {
        mainProgram();
    }

    public static void mainProgram() {
        Scanner scan = new Scanner(System.in);
        int n = scan.nextInt();
        int m = scan.nextInt();
        SNM clans = new SNM(n);
        for (int i = 0; i < m; i++) {
            String str = scan.next();
            if (str.equals("add")) {
                int x = scan.nextInt();
                int v = scan.nextInt();
                clans.add(x, v);
            } else if (str.equals("join")) {
                int x = scan.nextInt();
                int y = scan.nextInt();
                clans.join(x, y);
            } else if (str.equals("get")) {
                int x = scan.nextInt();
                System.out.println(clans.get(x));
            }
        }
        scan.close();
    }
}
class SNM {
    int[] value;
    int[] parent;
    int[] rank;

    SNM(int n){
        value = new int[n + 1];
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
            value[b] -= value[a];
            rank[a] = Math.max(rank[a], rank[b] + 1);
        }
    }

    public void add(int a, int value) {
        this.value[findParent(a)] += value;
    }

    public int get(int a) {
        if (parent[a] == a) {
            return value[a];
        } else {
            return value[a] + get(parent[a]);
        }
    }
}
