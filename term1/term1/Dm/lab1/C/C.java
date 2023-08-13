import java.io.*;
import java.util.*;

public class C {
    public static void main(String[] args) throws  IOException {
        mainProgram();
    }

    public static void mainProgram() throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        int n = Integer.parseInt(reader.readLine());
        FuncDiagram funcs = new FuncDiagram();
        for (int i = 0; i < n; i++) {
            StringBuilder str = new StringBuilder(reader.readLine());
            int m = Integer.parseInt(str.substring(0, 1));
            if (m == 0) {
                FuncElement func = new FuncElement();
                func.number = i + 1;
                func.isLeaf = true;
                funcs.addElement(func);
                continue;
            }
            FuncElement func = new FuncElement();

            int index = 0;func.number = i + 1;
            func.isLeaf = false;
            func.child = new int[m];
            func.result = new boolean[(int) Math.pow(2, m)];
            for (String string : str.substring(1, str.length()).split(" ")) {
                if (!string.isEmpty()) {
                    func.child[index++] = Integer.parseInt(string);
                }
            }
            str = new StringBuilder(reader.readLine());
            index = 0;
            for (String string : str.toString().split(" ")) {
                if (!string.isEmpty()) {
                    func.result[index++] = (Integer.parseInt(string) == 1);
                }
            }
            funcs.addElement(func);
        }
        reader.close();
        funcs.searchDepth();
        System.out.println(funcs.depth);
        for (int i = 0; i < (int) Math.pow(2, funcs.leafs.size()); i++) {
            System.out.print((funcs.getValue()) ? 1 : 0);
            funcs.changeValues();
        }
    }

//    static class FuncDiagram {
//        final HashMap<Integer, FuncElement> elements;
//        final HashMap<Integer, FuncElement> leafs;
//        final List<Integer> list = new ArrayList<>();
//        int depth;
//        int head;
//
//        FuncDiagram() {
//            this.elements = new HashMap<>();
//            this.leafs = new HashMap<>();
//            this.depth = 0;
//            this.head = 0;
//        }
//
//
//        public  void changeValues() {
//            for (int i = list.size() - 1; i >= 0; i--) {
//                if (!leafs.get(list.get(i)).value) {
//                    leafs.get(list.get(i)).value = true;
//                    break;
//                } else {
//                    leafs.get(list.get(i)).value = false;
//                }
//            }
//        }
//
//        public void addElement(FuncElement el) {
//            elements.put(el.number, el);
//            if (el.isLeaf) {
//                leafs.put(el.number, el);
//                list.add(el.number);
//            } else {
//                head = Math.max(el.number, head);
//            }
//        }
//
//        public void searchDepth() {
//            search(0, elements.get(head));
//        }
//
//        private void search(int depth, FuncElement el) {
//            this.depth = Math.max(this.depth, depth);
//            if (!el.isLeaf) {
//                for (int i = 0; i < el.child.length; i++) {
//                    search(depth + 1, elements.get(el.child[i]));
//                }
//            }
//        }
//
//        public boolean getValue() {
//            return getChildValue(elements.get(head));
//        }
//
//        private boolean getChildValue(FuncElement el) {
//            if (el.isLeaf) {
//                return el.value;
//            }
//            int index = 0;
//            int pow = (int) Math.pow(2, el.child.length - 1);
//            for (int i = 0; i < el.child.length; i++) {
//                index += (getChildValue(elements.get(el.child[i]))) ? pow : 0;
//                pow /= 2;
//            }
//            return el.result[index];
//        }
//    }


    static class FuncDiagram {
        final List<FuncElement> elements;
        final List<FuncElement> leafs;
        int depth;
        int head;

        FuncDiagram() {
            this.elements = new ArrayList<>();
            this.leafs = new ArrayList<>();
            this.depth = 0;
            this.head = 0;
        }


        public  void changeValues() {
            for (int i = leafs.size() - 1; i >= 0; i--) {
                if (!leafs.get(i).value) {
                    leafs.get(i).value = true;
                    break;
                } else {
                    leafs.get(i).value = false;
                }
            }
        }

        public void addElement(FuncElement el) {
            elements.add(el);
            if (el.isLeaf) {
                leafs.add(el);
            } else {
                head = Math.max(el.number, head);
            }
        }

        public void searchDepth() {
            search(0, elements.get(head - 1));
        }

        private void search(int depth, FuncElement el) {
            this.depth = Math.max(this.depth, depth);
            if (!el.isLeaf) {
                for (int i = 0; i < el.child.length; i++) {
                    search(depth + 1, elements.get(el.child[i] - 1));
                }
            }
        }

        public boolean getValue() {
            return getChildValue(elements.get(head - 1));
        }

        private boolean getChildValue(FuncElement el) {
            if (el.isLeaf) {
                return el.value;
            }
            int index = 0;
            int pow = (int) Math.pow(2, el.child.length - 1);
            for (int i = 0; i < el.child.length; i++) {
                index += (getChildValue(elements.get(el.child[i] - 1))) ? pow : 0;
                pow /= 2;
            }
            return el.result[index];
        }
    }
//    static class FuncElement {
//
//        final int number;
//        final int[] child;
//        final boolean[] result;
//        final boolean isLeaf;
//        boolean value;
//        FuncElement(int number, int[] child, boolean[] result) {
//            this.number = number;
//            this.child = child;
//            this.result = result;
//            this.isLeaf = false;
//        }
//
//        FuncElement(int number) {
//            this.number = number;
//            this.child = null;
//            this.result = null;
//            this.isLeaf = true;
//        }
//    }

    static class FuncElement {

        int number;
        int[] child;
        boolean[] result;
        boolean isLeaf;
        boolean value;
    }
}
