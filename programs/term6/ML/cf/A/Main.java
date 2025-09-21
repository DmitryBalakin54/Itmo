import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;
import java.util.*;

public class Main {

    static List<List<Integer>> gen(int l) {
        List<List<Integer>> res = new ArrayList<>();
        genall(l, 0, new ArrayList<>(), res);
        return res;
    }

    static void genall(int l, int curL, List<Integer> vv, List<List<Integer>> res) {
        if (curL == l) {
            res.add(new ArrayList<>(vv));
            return;
        }
        vv.add(0);
        genall(l, curL + 1, vv, res);
        vv.removeLast();

        vv.add(1);
        genall(l, curL + 1, vv, res);
        vv.removeLast();
    }

    static void print(List<Integer> ind, List<List<Integer>> vv) {
        for (int i : ind) {
            int cnt = 0;
            for (int b : vv.get(i)) {
                if (b == 1) {
                    System.out.print("1 ");
                    cnt++;
                } else {
                    System.out.print("-1 ");
                }
            }
            System.out.println(0.5 - cnt);
        }
    }

    public static void main(String[] args) throws IOException {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        int m = Integer.parseInt(br.readLine());
        List<List<Integer>> vv = gen(m);
        int sz = vv.size();

        List<Integer> one = new ArrayList<>();
        List<Integer> z = new ArrayList<>();

        for (int i = 0; i < sz; i++) {
            int v = Integer.parseInt(br.readLine());
            if (v == 0) z.add(i);
            else one.add(i);
        }

        if (one.size() > 512) {
            System.out.println("2");
            System.out.println(z.size() + " 1");
            print(z, vv);
            for (int i = 0; i < z.size(); i++) {
                System.out.print("-1 ");
            }
            System.out.println("0.5");
        } else {
            if (one.isEmpty()) {
                System.out.println("1");
                System.out.println("1");
                for (int i = 0; i < m; i++) {
                    System.out.print("0 ");
                }
                System.out.println("-0.5");
                return;
            }
            System.out.println("2");
            System.out.println(one.size() + " 1");
            print(one, vv);
            for (int i = 0; i < one.size(); i++) {
                System.out.print("1 ");
            }
            System.out.println("-0.5");
        }
    }
}
