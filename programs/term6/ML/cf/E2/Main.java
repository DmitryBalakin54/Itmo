import java.util.*;

public class Main {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        Network net = new Network();
        int n = scanner.nextInt();
        for (int i = 0; i < 4; ++i) {
            for (int t = 0; t < 2; ++t) {
                List<List<Double>> matr = new ArrayList<>();
                for (int j = 0; j < n; ++j) {
                    List<Double> row = new ArrayList<>();
                    for (int k = 0; k < n; ++k) {
                        row.add(scanner.nextDouble());
                    }
                    matr.add(row);
                }
                Var matrNode = new Var();
                matrNode.setData(matr);
                net.addNode(matrNode);
            }

            List<List<Double>> b = new ArrayList<>();
            for (int j = 0; j < n; ++j) {
                List<Double> row = new ArrayList<>();
                row.add(scanner.nextDouble());
                b.add(row);
            }
            Var bNode = new Var();
            bNode.setData(b);
            net.addNode(bNode);
        }
        int m = scanner.nextInt();
        for (int i = 0; i < 2; ++i) {
            List<List<Double>> vec = new ArrayList<>();
            for (int j = 0; j < n; ++j) {
                List<Double> row = new ArrayList<>();
                row.add(scanner.nextDouble());
                vec.add(row);
            }
            Var vecNode = new Var();
            vecNode.setData(vec);
            net.addNode(vecNode);
        }

        final int NODES_START = 4 * 3 + 2;
        final int NODE_SIZE = 1 + 3 * 4 + 8;
        Collections.swap(net.nodes, NODES_START - 1, NODES_START - 2);
        List<Integer> os = new ArrayList<>(List.of(-1000000000));
        List<Integer> cs = new ArrayList<>(List.of(NODES_START - 2));
        List<Integer> hs = new ArrayList<>(List.of(NODES_START - 1));
        List<Integer> xs = new ArrayList<>(List.of(-1000000000));
        for (int i = 0; i < m; ++i) {
            List<List<Double>> input = new ArrayList<>();
            for (int j = 0; j < n; ++j) {
                List<Double> row = new ArrayList<>();
                row.add(scanner.nextDouble());
                input.add(row);
            }
            Var inputNode = new Var();
            inputNode.setData(input);
            net.addNode(inputNode);
            int startPos = NODES_START + NODE_SIZE * i;
            int prevHPos = startPos - 1;
            int prevCPos = startPos - 2;
            int[] sums = new int[4];
            for (int j = 0; j < 4; ++j) {
                int WxPos = net.nodes.size();
                net.addNode(new Mul(net.getNode(j * 3), inputNode));
                int UhPos = net.nodes.size();
                net.addNode(new Mul(net.getNode(j * 3 + 1), net.getNode(prevHPos)));
                sums[j] = net.nodes.size();
                net.addNode(new Sum(Arrays.asList(net.getNode(WxPos), net.getNode(UhPos), net.getNode(j * 3 + 2))));
            }
            int fPos = net.nodes.size();
            net.addNode(new Sigm(net.getNode(sums[0])));
            int iPos = net.nodes.size();
            net.addNode(new Sigm(net.getNode(sums[1])));
            int oPos = net.nodes.size();
            net.addNode(new Sigm(net.getNode(sums[2])));
            int tahnPos = net.nodes.size();
            net.addNode(new Tnh(net.getNode(sums[3])));
            int ithanPos = net.nodes.size();
            net.addNode(new Had(Arrays.asList(net.getNode(iPos), net.getNode(tahnPos))));
            int fprevCPos = net.nodes.size();
            net.addNode(new Had(Arrays.asList(net.getNode(fPos), net.getNode(prevCPos))));
            int curCPos = net.nodes.size();
            net.addNode(new Sum(Arrays.asList(net.getNode(fprevCPos), net.getNode(ithanPos))));
            int curHPos = net.nodes.size();
            net.addNode(new Had(Arrays.asList(net.getNode(oPos), net.getNode(curCPos))));
            os.add(oPos);
            cs.add(curCPos);
            hs.add(curHPos);
            xs.add(startPos);
        }


        net.compute();
        net.getNode(hs.get(m)).readDiff(scanner);
        net.getNode(cs.get(m)).readDiff(scanner);
        for (int i = m; i >= 1; --i) {
            net.getNode(os.get(i)).readDiff(scanner);
        }
        net.backprop();

        for (int i = 1; i <= m; ++i) {
            net.printNode(os.get(i));
        }
        net.printNode(hs.get(m));
        net.printNode(cs.get(m));
        for (int i = m; i >= 1; --i) {
            net.printDiff(xs.get(i));
        }
        net.printDiff(hs.getFirst());
        net.printDiff(cs.getFirst());
        for (int i = 0; i < 4 * 3; ++i) {
            net.printDiff(i);
        }
    }


    abstract static class Node {
        List<Node> inputs;
        List<List<Double>> value;
        List<List<Double>> diff;

        public Node(List<Node> inputs) {
            this.inputs = inputs;
            this.value = new ArrayList<>();
            this.diff = new ArrayList<>();
        }

        abstract void compute();

        abstract void spreadDiff();

        void initializeDiff() {
            diff = new ArrayList<>();
            for (int i = 0; i < value.size(); i++) {
                List<Double> row = new ArrayList<>();
                for (int j = 0; j < value.get(0).size(); j++) {
                    row.add(0.0);
                }
                diff.add(row);
            }
        }

        void readDiff(Scanner scanner) {
            for (List<Double> doubles : diff) {
                for (int j = 0; j < diff.getFirst().size(); j++) {
                    doubles.set(j, scanner.nextDouble());
                }
            }
        }
    }

    static class Var extends Node {
        public Var() {
            super(new ArrayList<>());
        }

        public void setData(List<List<Double>> data) {
            value = new ArrayList<>();
            for (List<Double> row : data) {
                value.add(new ArrayList<>(row));
            }
        }

        @Override
        void compute() {
            initializeDiff();
        }

        @Override
        void spreadDiff() {
        }
    }

    static class Tnh extends Node {
        public Tnh(Node source) {
            super(Collections.singletonList(source));
        }

        @Override
        void compute() {
            value = new ArrayList<>();
            for (List<Double> row : inputs.getFirst().value) {
                List<Double> newRow = new ArrayList<>();
                for (Double cell : row) {
                    newRow.add(Math.tanh(cell));
                }
                value.add(newRow);
            }
            initializeDiff();
        }

        @Override
        void spreadDiff() {
            for (int i = 0; i < value.size(); i++) {
                for (int j = 0; j < value.getFirst().size(); j++) {
                    double curValue = value.get(i).get(j);
                    double delta = (1 - curValue * curValue) * diff.get(i).get(j);
                    inputs.getFirst().diff.get(i).set(j, inputs.getFirst().diff.get(i).get(j) + delta);
                }
            }
        }
    }

    static class Sigm extends Node {
        public Sigm(Node source) {
            super(Collections.singletonList(source));
        }

        @Override
        void compute() {
            value = new ArrayList<>();
            for (List<Double> row : inputs.getFirst().value) {
                List<Double> newRow = new ArrayList<>();
                for (Double cell : row) {
                    newRow.add(1.0 / (1 + Math.exp(-cell)));
                }
                value.add(newRow);
            }
            initializeDiff();
        }

        @Override
        void spreadDiff() {
            for (int i = 0; i < value.size(); i++) {
                for (int j = 0; j < value.getFirst().size(); j++) {
                    double curValue = value.get(i).get(j);
                    double delta = curValue * (1 - curValue) * diff.get(i).get(j);
                    inputs.getFirst().diff.get(i).set(j, inputs.getFirst().diff.get(i).get(j) + delta);
                }
            }
        }
    }

    static class Mul extends Node {
        public Mul(Node a, Node b) {
            super(Arrays.asList(a, b));
        }

        @Override
        void compute() {
            List<List<Double>> a = inputs.get(0).value;
            List<List<Double>> b = inputs.get(1).value;
            int n = a.size();
            int m = a.getFirst().size();
            int k = b.getFirst().size();
            value = new ArrayList<>();
            for (int i = 0; i < n; i++) {
                List<Double> row = new ArrayList<>();
                for (int j = 0; j < k; j++) {
                    double sum = 0;
                    for (int t = 0; t < m; t++) {
                        sum += a.get(i).get(t) * b.get(t).get(j);
                    }
                    row.add(sum);
                }
                value.add(row);
            }
            initializeDiff();
        }

        @Override
        void spreadDiff() {
            List<List<Double>> a = inputs.get(0).value;
            List<List<Double>> b = inputs.get(1).value;
            List<List<Double>> da = inputs.get(0).diff;
            List<List<Double>> db = inputs.get(1).diff;
            int n = a.size();
            int m = a.get(0).size();
            int k = b.get(0).size();
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < m; j++) {
                    double curDiff = 0;
                    for (int t = 0; t < k; t++) {
                        curDiff += diff.get(i).get(t) * b.get(j).get(t);
                    }
                    da.get(i).set(j, da.get(i).get(j) + curDiff);
                }
            }
            for (int i = 0; i < m; i++) {
                for (int j = 0; j < k; j++) {
                    double curDiff = 0;
                    for (int t = 0; t < n; t++) {
                        curDiff += a.get(t).get(i) * diff.get(t).get(j);
                    }
                    db.get(i).set(j, db.get(i).get(j) + curDiff);
                }
            }
        }
    }

    static class Sum extends Node {
        public Sum(List<Node> inputs) {
            super(inputs);
        }

        @Override
        void compute() {
            int n = inputs.getFirst().value.size();
            int m = inputs.getFirst().value.getFirst().size();
            value = new ArrayList<>();
            for (int i = 0; i < n; i++) {
                List<Double> row = new ArrayList<>();
                for (int j = 0; j < m; j++) {
                    row.add(0.0);
                }
                value.add(row);
            }
            for (Node input : inputs) {
                for (int i = 0; i < n; i++) {
                    for (int j = 0; j < m; j++) {
                        value.get(i).set(j, value.get(i).get(j) + input.value.get(i).get(j));
                    }
                }
            }
            initializeDiff();
        }

        @Override
        void spreadDiff() {
            for (int i = 0; i < value.size(); i++) {
                for (int j = 0; j < value.getFirst().size(); j++) {
                    for (Node input : inputs) {
                        input.diff.get(i).set(j, input.diff.get(i).get(j) + diff.get(i).get(j));
                    }
                }
            }
        }
    }

    static class Had extends Node {
        public Had(List<Node> inputs) {
            super(inputs);
        }

        @Override
        void compute() {
            int n = inputs.getFirst().value.size();
            int m = inputs.getFirst().value.getFirst().size();
            value = new ArrayList<>();
            for (int i = 0; i < n; i++) {
                List<Double> row = new ArrayList<>();
                for (int j = 0; j < m; j++) {
                    row.add(1.0);
                }
                value.add(row);
            }
            for (Node input : inputs) {
                for (int i = 0; i < n; i++) {
                    for (int j = 0; j < m; j++) {
                        value.get(i).set(j, value.get(i).get(j) * input.value.get(i).get(j));
                    }
                }
            }
            initializeDiff();
        }

        @Override
        void spreadDiff() {
            for (int i = 0; i < value.size(); i++) {
                for (int j = 0; j < value.get(0).size(); j++) {
                    for (int k = 0; k < inputs.size(); k++) {
                        double multiplier = 1;
                        for (int t = 0; t < inputs.size(); t++) {
                            if (t != k) {
                                multiplier *= inputs.get(t).value.get(i).get(j);
                            }
                        }
                        inputs.get(k).diff.get(i).set(j, inputs.get(k).diff.get(i).get(j) + multiplier * diff.get(i).get(j));
                    }
                }
            }
        }
    }

    static class Network {
        List<Node> nodes = new ArrayList<>();

        Node getNode(int pos) {
            return nodes.get(pos);
        }

        void addNode(Node newNode) {
            nodes.add(newNode);
        }

        void printNode(int idx) {
            Node node = nodes.get(idx);
            for (List<Double> row : node.value) {
                for (Double cell : row) {
                    System.out.printf("%.12f ", cell);
                }
                System.out.println();
            }
        }

        void printDiff(int idx) {
            Node node = nodes.get(idx);
            for (List<Double> row : node.diff) {
                for (Double cell : row) {
                    System.out.printf("%.12f ", cell);
                }
                System.out.println();
            }
        }

        void compute() {
            for (Node node : nodes) {
                node.compute();
            }
        }

        void backprop() {
            for (int i = nodes.size() - 1; i >= 0; i--) {
                nodes.get(i).spreadDiff();
            }
        }
    }
}