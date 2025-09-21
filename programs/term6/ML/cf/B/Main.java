import java.io.*;
import java.util.*;
import java.util.function.*;

public class Main {
    record Matrix(int rowC, int colC, List<List<Double>> matrix) {

        Matrix mapEl(DoubleUnaryOperator f) {
                List<List<Double>> newMat = new ArrayList<>();
                for (List<Double> row : matrix) {
                    List<Double> roww = new ArrayList<>();
                    for (Double el : row) {
                        roww.add(f.applyAsDouble(el));
                    }
                    newMat.add(roww);
                }
                return new Matrix(rowC, colC, newMat);
            }

            Matrix mapElIdx(F f) {
                List<List<Double>> newMatrix = new ArrayList<>();
                for (int row = 0; row < rowC; row++) {
                    List<Double> newRow = new ArrayList<>();
                    for (int column = 0; column < colC; column++) {
                        newRow.add(f.apply(row, column, matrix.get(row).get(column)));
                    }
                    newMatrix.add(newRow);
                }
                return new Matrix(rowC, colC, newMatrix);
            }

            Matrix mul(Matrix another, boolean transpose, boolean transposeAnother) {
                int m = !transpose ? colC : rowC;
                int m1 = !transposeAnother ? another.rowC : another.colC;
                int n = !transpose ? rowC : colC;
                int k = !transposeAnother ? another.colC : another.rowC;

                List<List<Double>> resMatrix = new ArrayList<>();
                for (int i = 0; i < n; i++) {
                    List<Double> row = new ArrayList<>();
                    for (int j = 0; j < k; j++) {
                        row.add(0.0);
                    }
                    resMatrix.add(row);
                }

                for (int i = 0; i < n; i++) {
                    for (int j = 0; j < k; j++) {
                        for (int l = 0; l < m; l++) {
                            double left = !transpose ? matrix.get(i).get(l) : matrix.get(l).get(i);
                            double right = !transposeAnother ? another.matrix.get(l).get(j) : another.matrix.get(j).get(l);
                            resMatrix.get(i).set(j, resMatrix.get(i).get(j) + left * right);
                        }
                    }
                }
                return new Matrix(n, k, resMatrix);
            }

            Matrix mul(Matrix another) {
                return mul(another, false, false);
            }

            Matrix plus(Matrix another) {
                return mapElIdx((row, column, value) -> value + another.matrix.get(row).get(column));
            }

            Matrix adMul(Matrix another) {
                return mapElIdx((row, column, value) -> value * another.matrix.get(row).get(column));
            }

            void print() {
                for (int i = 0; i < rowC; i++) {
                    for (int j = 0; j < colC; j++) {
                        System.out.printf("%.8f ", matrix.get(i).get(j));
                    }
                    System.out.println();
                }
            }

            static Matrix read(int rowCnt, int columnCnt, BufferedReader reader) throws IOException {
                List<List<Double>> matrix = new ArrayList<>();
                for (int i = 0; i < rowCnt; i++) {
                    matrix.add(readD(reader));
                }
                return new Matrix(rowCnt, columnCnt, matrix);
            }

            static Matrix get(int rowCnt, int columnCnt, double value) {
                List<List<Double>> resMatrix = new ArrayList<>();
                for (int i = 0; i < rowCnt; i++) {
                    List<Double> row = new ArrayList<>();
                    for (int j = 0; j < columnCnt; j++) {
                        row.add(value);
                    }
                    resMatrix.add(row);
                }
                return new Matrix(rowCnt, columnCnt, resMatrix);
            }
        }

    interface F {
        double apply(int row, int column, double value);
    }

    abstract static class Node {
        private Matrix fc;
        protected Matrix d;

        abstract Matrix calc();
        abstract void psh();

        Matrix calcF() {
            if (fc == null) {
                fc = calc();
            }
            return fc;
        }

        private Matrix eye() {
            Matrix functionMatrix = calcF();
            return Matrix.get(functionMatrix.rowC, functionMatrix.colC, 0.0);
        }

        Matrix getD() {
            return d == null ? eye() : d;
        }

        void addD(Matrix diff) {
            if (d == null) {
                d = diff;
            } else {
                d = d.plus(diff);
            }
        }

        static List<Node> read(int n, int m, BufferedReader reader) throws IOException {
            List<Node> res = new ArrayList<>();
            for (int i = 0; i < n; i++) {
                String[] data = reader.readLine().trim().split(" ");
                List<Integer> args = new ArrayList<>();
                for (int j = 1; j < data.length; j++) {
                    args.add(Integer.parseInt(data[j]));
                }
                switch (data[0]) {
                    case "var":
                        res.add(new VarNode(args.get(0), args.get(1)));
                        break;
                    case "tnh":
                        res.add(new TnhNode(res.get(args.getFirst() - 1)));
                        break;
                    case "rlu":
                        res.add(new RluNode(1.0 / args.get(0), res.get(args.get(1) - 1)));
                        break;
                    case "mul":
                        res.add(new MulNode(res.get(args.get(0) - 1), res.get(args.get(1) - 1)));
                        break;
                    case "sum":
                        int len = args.get(0);
                        List<Node> sumArgs = new ArrayList<>();
                        for (int j = 1; j < args.size(); j++) {
                            sumArgs.add(res.get(args.get(j) - 1));
                        }
                        res.add(new SumNode(len, sumArgs));
                        break;
                    case "had":
                        int hadLen = args.get(0);
                        List<Node> hadArgs = new ArrayList<>();
                        for (int j = 1; j < args.size(); j++) {
                            hadArgs.add(res.get(args.get(j) - 1));
                        }
                        res.add(new HadNode(hadLen, hadArgs));
                        break;
                }
            }

            for (int i = 0; i < m; i++) {
                VarNode inputNode = (VarNode) res.get(i);
                inputNode.matrix = Matrix.read(inputNode.rowCnt, inputNode.columnCnt, reader);
            }

            return res;
        }
    }

    static class VarNode extends Node {
        final int rowCnt;
        final int columnCnt;
        Matrix matrix;

        VarNode(int rowCnt, int columnCnt) {
            this.rowCnt = rowCnt;
            this.columnCnt = columnCnt;
        }

        @Override
        Matrix calc() {
            return matrix;
        }

        @Override
        void psh() {
        }
    }

    static class TnhNode extends Node {
        final Node argNode;

        TnhNode(Node argNode) {
            this.argNode = argNode;
        }

        @Override
        Matrix calc() {
            return argNode.calcF().mapEl(Math::tanh);
        }

        @Override
        void psh() {
            argNode.addD(calcF().mapElIdx((row, column, tnh) ->
                    (1.0 - tnh * tnh) * d.matrix.get(row).get(column)
            ));
        }
    }

    static class RluNode extends Node {
        final double alpha;
        final Node argNode;

        RluNode(double alpha, Node argNode) {
            this.alpha = alpha;
            this.argNode = argNode;
        }

        @Override
        Matrix calc() {
            return argNode.calcF().mapEl(x -> Math.max(x, alpha * x));
        }

        @Override
        void psh() {
            argNode.addD(argNode.calcF().mapElIdx((row, column, x) ->
                    d.matrix.get(row).get(column) * (x < 0.0 ? alpha : 1.0)
            ));
        }
    }

    static class MulNode extends Node {
        final Node l;
        final Node r;

        MulNode(Node l, Node r) {
            this.l = l;
            this.r = r;
        }

        @Override
        Matrix calc() {
            return l.calcF().mul(r.calcF());
        }

        @Override
        void psh() {
            l.addD(d.mul(r.calcF(), false, true));
            r.addD(l.calcF().mul(d, true, false));
        }
    }

    static class SumNode extends Node {
        final int l;
        final List<Node> vals;

        SumNode(int l, List<Node> vals) {
            this.l = l;
            this.vals = vals;
        }

        @Override
        Matrix calc() {
            Matrix result = vals.get(0).calcF();
            for (int i = 1; i < vals.size(); i++) {
                result = result.plus(vals.get(i).calcF());
            }
            return result;
        }

        @Override
        void psh() {
            for (Node arg : vals) {
                arg.addD(d);
            }
        }
    }

    static class HadNode extends Node {
        final int l;
        final List<Node> vals;

        HadNode(int l, List<Node> vals) {
            this.l = l;
            this.vals = vals;
        }

        @Override
        Matrix calc() {
            Matrix result = vals.get(0).calcF();
            for (int i = 1; i < vals.size(); i++) {
                result = result.adMul(vals.get(i).calcF());
            }
            return result;
        }

        @Override
        void psh() {
            for (int argInd = 0; argInd < vals.size(); argInd++) {
                Matrix ex = vals.getFirst().calcF();
                Matrix acc = Matrix.get(ex.rowC, ex.colC, 1.0);
                for (int ind = 0; ind < vals.size(); ind++) {
                    if (ind != argInd) {
                        acc = acc.adMul(vals.get(ind).calcF());
                    }
                }
                vals.get(argInd).addD(acc.adMul(d));
            }
        }
    }

    static List<String> readS(BufferedReader reader) throws IOException {
        return Arrays.asList(reader.readLine().trim().split(" "));
    }

    static List<Integer> readPars(BufferedReader reader) throws IOException {
        List<String> strings = readS(reader);
        List<Integer> ints = new ArrayList<>();
        for (String s : strings) {
            ints.add(Integer.parseInt(s));
        }
        return ints;
    }

    static List<Double> readD(BufferedReader reader) throws IOException {
        List<String> strings = readS(reader);
        List<Double> doubles = new ArrayList<>();
        for (String s : strings) {
            doubles.add(Double.parseDouble(s));
        }
        return doubles;
    }

    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        List<Integer> pars = readPars(reader);
        int n = pars.get(0);
        int m = pars.get(1);
        int k = pars.get(2);

        List<Node> nodes = Node.read(n, m, reader);

        for (int i = nodes.size() - k; i < nodes.size(); i++) {
            Matrix resMatrix = nodes.get(i).calcF();
            resMatrix.print();
            nodes.get(i).addD(Matrix.read(resMatrix.rowC, resMatrix.colC, reader));
        }

        for (int i = nodes.size() - 1; i >= 0; i--) {
            nodes.get(i).psh();
        }

        for (int i = 0; i < m; i++) {
            nodes.get(i).getD().print();
        }
    }
}