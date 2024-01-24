import java.util.*;

public class K1 {
    static boolean isTopSort(int n, List<Integer>[] lst, List<Integer> perm) {
        int[] ord = new int[n];
        for (int i = 0; i < n; i++) {
            ord[i] = -1;
        }

        for (int i = 0; i < n; i++) {
            ord[perm.get(i)] = i;
        }

        for (int i = 0; i < n; i++) {
            for (int neighbor : lst[i]) {
                if (ord[i] > ord[neighbor]) {
                    return false;
                }
            }
        }

        return true;
    }

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        int n = scanner.nextInt();
        int m = scanner.nextInt();

        List<Integer>[] lst = new ArrayList[n];
        for (int i = 0; i < n; i++) {
            lst[i] = new ArrayList<>();
        }

        for (int i = 0; i < m; i++) {
            int u = scanner.nextInt() - 1;
            int v = scanner.nextInt() - 1;
            lst[u].add(v);
        }

        List<Integer> perm = new ArrayList<>();
        for (int i = 0; i < n; i++) {
            perm.add(scanner.nextInt() - 1);
        }

        if (isTopSort(n, lst, perm)) {
            System.out.println("YES");
        } else {
            System.out.println("NO");
        }
    }
}
