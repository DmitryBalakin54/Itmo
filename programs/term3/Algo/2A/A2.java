import java.util.ArrayList;
import java.util.Collections;
import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStreamReader;

public class A2 {
static class Graph {

    private static class Edge {
        private final int ind;
        private final int wes;

        public Edge(int i, int w) {
            ind = i;
            wes = w;
        }

        public int getInd() {
            return ind;
        }

        public int getWes() {
            return wes;
        }
    }

    private final ArrayList<ArrayList<Edge>> mat;

    public Graph(int n) {
        mat = new ArrayList<>(n);
        for (int i = 0; i < n; i++) {
            mat.add(new ArrayList<>());
        }
    }

    public int size() {
        return mat.size();
    }

    public void add(int i, int j, int w) {
        mat.get(i).add(new Edge(j, w));
    }

    public Graph T() {
        Graph res = new Graph(size());
        for (int i = 0; i < size(); i++) {
            for (int j = 0; j < mat.get(i).size(); j++) {
                res.add(mat.get(i).get(j).getInd(), i, mat.get(i).get(j).getWes());
            }
        }
        return res;
    }

    public ArrayList<Integer> get(int[] k) {
        ArrayList<Integer> ord = new ArrayList<>();
        ArrayList<Integer> res = new ArrayList<>(Collections.nCopies(size(), 0));
        ArrayList<Integer> col = new ArrayList<>(Collections.nCopies(size(), 0));
        for (int i = 0; i < size(); i++) {
            dfsOne(i, col, ord);
        }
        col = new ArrayList<>(Collections.nCopies(size(), 0));
        Graph tG = T();
        for (int i = 0; i < size(); i++) {
            ArrayList<Integer> sss = new ArrayList<>();
            tG.dfsTwo(ord.get(size() - i - 1), col, sss);
            for(Integer intVal: sss){
                res.set(intVal, k[0]);
            }
            if (!sss.isEmpty()) {
                k[0]++;
            }

            sss.clear();
        }
        return res;
    }

    private void dfsOne(int i, ArrayList<Integer> colors, ArrayList<Integer> ord) {
        if (colors.get(i) != 0) {
            return;
        }
        colors.set(i, 1);
        for (Edge e : mat.get(i)) {
            dfsOne(e.getInd(), colors, ord);
        }
        ord.add(i);
    }

    private void dfsTwo(int i, ArrayList<Integer> col, ArrayList<Integer> sss) {
        if (col.get(i) != 0) {
            return;
        }
        col.set(i, 1);
        sss.add(i);
        for (Edge e : mat.get(i)) {
            dfsTwo(e.getInd(), col, sss);
        }
    }
}


    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        String[] nm = reader.readLine().split(" ");
        int n = Integer.parseInt(nm[0]);
        int m = Integer.parseInt(nm[1]);

        Graph graph = new Graph(n);
        for (int i = 0; i < m; i++) {
            String[] ab = reader.readLine().split(" ");
            int a = Integer.parseInt(ab[0]);
            int b = Integer.parseInt(ab[1]);
            graph.add(a - 1, b - 1, 0);
        }

        int[] k = {0};
        ArrayList<Integer> t = graph.get(k);
        System.out.println(k[0]);

        for (Integer integer : t) {
            System.out.print(integer + 1 + " ");
        }
    }
}
