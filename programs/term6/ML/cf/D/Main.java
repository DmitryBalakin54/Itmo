import java.io.*;
import java.util.*;

public class Main {
    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        List<Node> nodes = Node.readNodes(reader);
        Node outputNode = nodes.getLast();
        Matrix3D layers = outputNode.calcFunction();
        for (int i = 0; i < layers.depth; i++) {
            layers.get(i).print();
        }
        System.out.println();
        outputNode.addDeriv(Matrix3D.parseMatrix3D(layers.depth, layers.dim, readDoubles(reader)));
        ListIterator<Node> it = nodes.listIterator(nodes.size());
        while (it.hasPrevious()) {
            it.previous().pushDeriv();
        }
        for (SquareMatrix m : nodes.getFirst().getDeriv().getLayers()) {
            m.print();
        }
        System.out.println();
        for (Node n : nodes) {
            if (n instanceof NodeWithMutableParams) {
                ((NodeWithMutableParams) n).printParamDeriv();
            }
        }
    }

    static String readLn(BufferedReader reader) throws IOException {
        return reader.readLine().trim();
    }

    static int readInt(BufferedReader reader) throws IOException {
        return Integer.parseInt(readLn(reader));
    }

    static List<String> readStrings(BufferedReader reader) throws IOException {
        return Arrays.asList(readLn(reader).split(" "));
    }
    static List<Double> readDoubles(BufferedReader reader) throws IOException {
        List<String> parts = readStrings(reader);
        List<Double> res = new ArrayList<>();
        for (String s : parts) res.add(Double.parseDouble(s));
        return res;
    }

    static class SquareMatrix {
        private final List<List<Double>> source;
        final int dim;

        SquareMatrix(List<List<Double>> source) {
            this.source = source;
            this.dim = source.size();
        }

        SquareMatrix mapElem(java.util.function.Function<Double, Double> f) {
            List<List<Double>> newSource = new ArrayList<>();
            for (List<Double> row : source) {
                List<Double> newRow = new ArrayList<>();
                for (Double elem : row) {
                    newRow.add(f.apply(elem));
                }
                newSource.add(newRow);
            }
            return new SquareMatrix(newSource);
        }

        SquareMatrix mapElemIndexed(TriFunction<Integer, Integer, Double, Double> f) {
            List<List<Double>> newSource = new ArrayList<>();
            for (int row = 0; row < dim; row++) {
                List<Double> newRow = new ArrayList<>();
                for (int column = 0; column < dim; column++) {
                    newRow.add(f.apply(row, column, source.get(row).get(column)));
                }
                newSource.add(newRow);
            }
            return new SquareMatrix(newSource);
        }

        List<Double> get(int ind) {
            return source.get(ind);
        }

        SquareMatrix plus(SquareMatrix another) {
            List<List<Double>> anotherMatrix = another.source;
            return mapElemIndexed((row, column, value) -> value + anotherMatrix.get(row).get(column));
        }

        void print() {
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < dim; i++) {
                for (int j = 0; j < dim; j++) {
                    sb.append(String.format("%.1f ", source.get(i).get(j)));
                }
            }
            System.out.print(sb.toString().trim());
        }

        static SquareMatrix parseMatrix(int dim, List<Double> source) {
            List<List<Double>> matrix = new ArrayList<>();
            for (int i = 0; i < dim; i++) {
                matrix.add(new ArrayList<>(source.subList(dim * i, dim * (i + 1))));
            }
            return new SquareMatrix(matrix);
        }

        static SquareMatrix getValueMatrix(int dim, double value) {
            List<List<Double>> resMatrix = new ArrayList<>();
            for (int i = 0; i < dim; i++) {
                List<Double> row = new ArrayList<>();
                for (int j = 0; j < dim; j++) {
                    row.add(value);
                }
                resMatrix.add(row);
            }
            return new SquareMatrix(resMatrix);
        }

        List<List<Double>> getSource() {
            return source;
        }
    }

    @FunctionalInterface
    interface TriFunction<T, U, V, R> {
        R apply(T t, U u, V v);
    }

    static class Matrix3D {
        private final List<SquareMatrix> layers;
        final int depth;
        final int dim;
        final List<Integer> indices;

        Matrix3D(List<SquareMatrix> layers) {
            this.layers = layers;
            this.depth = layers.size();
            this.dim = layers.get(0).dim;
            this.indices = new ArrayList<>();
            for (int i = 0; i < depth; i++) indices.add(i);
        }

        List<SquareMatrix> getLayers() {
            return layers;
        }

        SquareMatrix get(int ind) {
            return layers.get(ind);
        }

        Matrix3D plus(Matrix3D other) {
            List<SquareMatrix> newLayers = new ArrayList<>();
            for (int i = 0; i < depth; i++) {
                newLayers.add(layers.get(i).plus(other.get(i)));
            }
            return new Matrix3D(newLayers);
        }

        Matrix3D map(java.util.function.Function<SquareMatrix, SquareMatrix> f) {
            List<SquareMatrix> newLayers = new ArrayList<>();
            for (SquareMatrix layer : layers) {
                newLayers.add(f.apply(layer));
            }
            return new Matrix3D(newLayers);
        }

        Matrix3D mapIndexed(BiFunction<Integer, SquareMatrix, SquareMatrix> f) {
            List<SquareMatrix> newLayers = new ArrayList<>();
            for (int i = 0; i < depth; i++) {
                newLayers.add(f.apply(i, layers.get(i)));
            }
            return new Matrix3D(newLayers);
        }

        void forEach(java.util.function.Consumer<SquareMatrix> f) {
            layers.forEach(f);
        }

        static Matrix3D parseMatrix3D(int depth, int dim, List<Double> source) {
            List<SquareMatrix> matrix = new ArrayList<>();
            int dimSqr = dim * dim;
            for (int i = 0; i < depth; i++) {
                matrix.add(SquareMatrix.parseMatrix(dim, source.subList(dimSqr * i, dimSqr * (i + 1))));
            }
            return new Matrix3D(matrix);
        }

        static Matrix3D getValueMatrix3D(int depth, int dim, double value) {
            List<SquareMatrix> resMatrix3D = new ArrayList<>();
            for (int i = 0; i < depth; i++) {
                resMatrix3D.add(SquareMatrix.getValueMatrix(dim, value));
            }
            return new Matrix3D(resMatrix3D);
        }
    }

    @FunctionalInterface
    interface BiFunction<T, U, R> {
        R apply(T t, U u);
    }

    interface NodeWithMutableParams {
        void printParamDeriv();
    }

    abstract static class Node {
        private Matrix3D functionCache;
        private Matrix3D myDeriv;

        protected abstract Matrix3D calcFunctionInner();
        abstract void pushDeriv();

        Matrix3D calcFunction() {
            if (functionCache == null) {
                functionCache = calcFunctionInner();
            }
            return functionCache;
        }

        private Matrix3D getZeroMatrix() {
            Matrix3D func = calcFunction();
            return Matrix3D.getValueMatrix3D(func.depth, func.dim, 0.0);
        }

        Matrix3D getDeriv() {
            return myDeriv == null ? getZeroMatrix() : myDeriv;
        }

        void addDeriv(Matrix3D deriv) {
            myDeriv = myDeriv == null ? deriv : myDeriv.plus(deriv);
        }

        static List<Node> readNodes(BufferedReader reader) throws IOException {
            List<String> inputMatrixInfo = readStrings(reader);
            int dim = Integer.parseInt(inputMatrixInfo.get(0));
            int depth = Integer.parseInt(inputMatrixInfo.get(1));
            List<Double> inputMatrix3D = new ArrayList<>();
            for (int i = 2; i < inputMatrixInfo.size(); i++) {
                inputMatrix3D.add(Double.parseDouble(inputMatrixInfo.get(i)));
            }
            Matrix3D matrix3D = Matrix3D.parseMatrix3D(depth, dim, inputMatrix3D);
            List<Node> res = new ArrayList<>();
            res.add(new VarNode(matrix3D));
            int l = readInt(reader);
            for (int i = 0; i < l; i++) {
                List<String> data = readStrings(reader);
                List<String> args = data.subList(1, data.size());
                Node prev = res.get(res.size() - 1);
                switch (data.get(0)) {
                    case "relu":
                        res.add(new ReluNode(1.0 / Integer.parseInt(args.get(0)), prev));
                        break;
                    case "pool":
                        res.add(new PoolNode(Integer.parseInt(args.get(0)), prev));
                        break;
                    case "bias":
                        List<Double> b = new ArrayList<>();
                        for (String arg : args) b.add(Double.parseDouble(arg));
                        res.add(new BiasNode(b, prev));
                        break;
                    default:
                        int h = Integer.parseInt(args.get(0));
                        int k = Integer.parseInt(args.get(1));
                        int s = Integer.parseInt(args.get(2));
                        int p = Integer.parseInt(args.get(3));
                        List<Double> kernel = new ArrayList<>();
                        for (int j = 4; j < args.size(); j++) {
                            kernel.add(Double.parseDouble(args.get(j)));
                        }
                        switch (data.get(0)) {
                            case "cnvm":
                                res.add(new CnvmNode(h, k, s, p, kernel, prev));
                                break;
                            case "cnve":
                                res.add(new CnveNode(h, k, s, p, kernel, prev));
                                break;
                            case "cnvc":
                                res.add(new CnvcNode(h, k, s, p, kernel, prev));
                                break;
                        }
                }
            }
            return res;
        }
    }

    static class VarNode extends Node {
        private final Matrix3D layers;

        VarNode(Matrix3D layers) {
            this.layers = layers;
        }

        @Override
        protected Matrix3D calcFunctionInner() {
            return layers;
        }

        @Override
        void pushDeriv() {
        }
    }

    static class ReluNode extends Node {
        private final double alpha;
        private final Node prev;

        ReluNode(double alpha, Node prev) {
            this.alpha = alpha;
            this.prev = prev;
        }

        @Override
        protected Matrix3D calcFunctionInner() {
            return prev.calcFunction().map(layer -> layer.mapElem(x -> Math.max(x, alpha * x)));
        }

        @Override
        void pushDeriv() {
            prev.addDeriv(prev.calcFunction().mapIndexed((layerInd, layer) -> {
                Matrix3D layerDeriv = getDeriv();
                return layer.mapElemIndexed((row, column, x) ->
                    layerDeriv.get(layerInd).get(row).get(column) * (x < 0.0 ? alpha : 1.0)
                );
            }));
        }
    }

    static class PoolNode extends Node {
        private final int sub;
        private final Node prev;

        PoolNode(int sub, Node prev) {
            this.sub = sub;
            this.prev = prev;
        }

        @Override
        protected Matrix3D calcFunctionInner() {
            return prev.calcFunction().map(layer -> {
                List<List<Double>> resMatrix = new ArrayList<>();
                int newDim = layer.dim / sub;
                for (int iIter = 0; iIter < newDim; iIter++) {
                    List<Double> row = new ArrayList<>();
                    for (int jIter = 0; jIter < newDim; jIter++) {
                        Double mxValue = null;
                        int iSt = iIter * sub;
                        int jSt = jIter * sub;
                        for (int i = 0; i < sub; i++) {
                            for (int j = 0; j < sub; j++) {
                                double cellVal = layer.get(iSt + i).get(jSt + j);
                                mxValue = mxValue == null ? cellVal : Math.max(mxValue, cellVal);
                            }
                        }
                        row.add(mxValue);
                    }
                    resMatrix.add(row);
                }
                return new SquareMatrix(resMatrix);
            });
        }

        @Override
        void pushDeriv() {
            Matrix3D layers = calcFunction();
            Matrix3D deriv = prev.calcFunction().mapIndexed((layerInd, prevLayer) -> {
                SquareMatrix prevLayerDeriv = SquareMatrix.getValueMatrix(prevLayer.dim, 0.0);
                SquareMatrix layerDeriv = getDeriv().get(layerInd);
                SquareMatrix layer = layers.get(layerInd);
                int dim = layer.dim;
                for (int iIter = 0; iIter < dim; iIter++) {
                    for (int jIter = 0; jIter < dim; jIter++) {
                        double mxValue = layer.get(iIter).get(jIter);
                        int iSt = iIter * sub;
                        int jSt = jIter * sub;
                        for (int i = 0; i < sub; i++) {
                            for (int j = 0; j < sub; j++) {
                                double cellVal = prevLayer.get(iSt + i).get(jSt + j);
                                if (cellVal == mxValue) {
                                    prevLayerDeriv.get(iSt + i).set(jSt + j, layerDeriv.get(iIter).get(jIter));
                                }
                            }
                        }
                    }
                }
                return prevLayerDeriv;
            });
            prev.addDeriv(deriv);
        }
    }

    static class BiasNode extends Node implements NodeWithMutableParams {
        private final List<Double> b;
        private final Node prev;

        BiasNode(List<Double> b, Node prev) {
            this.b = b;
            this.prev = prev;
        }

        @Override
        protected Matrix3D calcFunctionInner() {
            return prev.calcFunction().mapIndexed((layerInd, layer) -> {
                double add = b.get(layerInd);
                return layer.mapElem(x -> x + add);
            });
        }

        @Override
        void pushDeriv() {
            prev.addDeriv(getDeriv());
        }

        @Override
        public void printParamDeriv() {
            getDeriv().forEach(layer -> {
                double sum = 0.0;
                for (List<Double> row : layer.getSource()) {
                    for (double val : row) {
                        sum += val;
                    }
                }
                System.out.print(sum + " ");
            });
            System.out.println();
        }
    }

    abstract static class CnvxNode extends Node implements NodeWithMutableParams {
        private final int h;
        private final int k;
        private final int s;
        protected final int p;
        private final Node prev;
        private List<Matrix3D> kernel;
        private List<Matrix3D> kernelDeriv;
        private Matrix3D paddedLayersCache;

        CnvxNode(int h, int k, int s, int p, List<Double> unparsedKernel, Node prev) {
            this.h = h;
            this.k = k;
            this.s = s;
            this.p = p;
            this.prev = prev;
            this.kernel = parseKernel(unparsedKernel);
            this.kernelDeriv = initKernelDeriv();
        }

        private List<Matrix3D> parseKernel(List<Double> unparsedKernel) {
            int depth = prev.calcFunction().depth;
            List<Matrix3D> result = new ArrayList<>();
            int matrixElemCnt = depth * k * k;
            for (int layerInd = 0; layerInd < h; layerInd++) {
                result.add(Matrix3D.parseMatrix3D(
                    depth,
                    k,
                    unparsedKernel.subList(matrixElemCnt * layerInd, matrixElemCnt * (layerInd + 1))
                ));
            }
            return result;
        }

        private List<Matrix3D> initKernelDeriv() {
            int depth = kernel.get(0).depth;
            int dim = kernel.get(0).dim;
            List<Matrix3D> result = new ArrayList<>();
            for (Matrix3D k : kernel) {
                result.add(Matrix3D.getValueMatrix3D(depth, dim, 0.0));
            }
            return result;
        }

        protected abstract void fill(int dim, SquareMatrix matrix);

        private SquareMatrix getPaddedMatrix(SquareMatrix layer) {
            int dim = layer.dim;
            List<List<Double>> padded = new ArrayList<>();
            for (int i = 0; i < p; i++) {
                List<Double> row = new ArrayList<>();
                for (int j = 0; j < dim + 2 * p; j++) {
                    row.add(0.0);
                }
                padded.add(row);
            }
            for (int i = 0; i < dim; i++) {
                List<Double> row = new ArrayList<>();
                for (int j = 0; j < p; j++) {
                    row.add(0.0);
                }
                for (int j = 0; j < dim; j++) {
                    row.add(layer.get(i).get(j));
                }
                for (int j = 0; j < p; j++) {
                    row.add(0.0);
                }
                padded.add(row);
            }
            for (int i = 0; i < p; i++) {
                List<Double> row = new ArrayList<>();
                for (int j = 0; j < dim + 2 * p; j++) {
                    row.add(0.0);
                }
                padded.add(row);
            }
            SquareMatrix matrix = new SquareMatrix(padded);
            fill(dim, matrix);
            return matrix;
        }

        private Matrix3D getPaddedLayers() {
            if (paddedLayersCache == null) {
                paddedLayersCache = prev.calcFunction().map(this::getPaddedMatrix);
            }
            return paddedLayersCache;
        }

        @Override
        protected Matrix3D calcFunctionInner() {
            Matrix3D paddedLayers = getPaddedLayers();
            int oldLayersDepth = paddedLayers.depth;
            int dim = paddedLayers.dim;
            int newDim = (dim - k) / s + 1;
            List<SquareMatrix> newLayers = new ArrayList<>();
            for (int layerInd = 0; layerInd < h; layerInd++) {
                List<List<Double>> newLayer = new ArrayList<>();
                for (int iIter = 0; iIter < newDim; iIter++) {
                    List<Double> row = new ArrayList<>();
                    for (int jIter = 0; jIter < newDim; jIter++) {
                        double cellValue = 0.0;
                        int iSt = iIter * s;
                        int jSt = jIter * s;
                        for (int oldLayerInd = 0; oldLayerInd < oldLayersDepth; oldLayerInd++) {
                            for (int i = 0; i < k; i++) {
                                for (int j = 0; j < k; j++) {
                                    cellValue += paddedLayers.get(oldLayerInd).get(iSt + i).get(jSt + j) *
                                        kernel.get(layerInd).get(oldLayerInd).get(i).get(j);
                                }
                            }
                        }
                        row.add(cellValue);
                    }
                    newLayer.add(row);
                }
                newLayers.add(new SquareMatrix(newLayer));
            }
            return new Matrix3D(newLayers);
        }

        @Override
        void pushDeriv() {
            Matrix3D paddedLayers = getPaddedLayers();
            Matrix3D oldLayers = prev.calcFunction();
            int oldDim = oldLayers.dim;
            int paddedDim = oldDim + 2 * p;
            Matrix3D pushDeriv = Matrix3D.getValueMatrix3D(oldLayers.depth, paddedDim, 0.0);
            int newDim = getDeriv().dim;
            for (int layerInd = 0; layerInd < h; layerInd++) {
                for (int iIter = 0; iIter < newDim; iIter++) {
                    for (int jIter = 0; jIter < newDim; jIter++) {
                        int iSt = iIter * s;
                        int jSt = jIter * s;
                        for (int oldLayerInd = 0; oldLayerInd < oldLayers.depth; oldLayerInd++) {
                            for (int i = 0; i < k; i++) {
                                for (int j = 0; j < k; j++) {
                                    pushDeriv.get(oldLayerInd).get(iSt + i).set(jSt + j,
                                        pushDeriv.get(oldLayerInd).get(iSt + i).get(jSt + j) +
                                        getDeriv().get(layerInd).get(iIter).get(jIter) *
                                        kernel.get(layerInd).get(oldLayerInd).get(i).get(j));
                                    kernelDeriv.get(layerInd).get(oldLayerInd).get(i).set(j,
                                        kernelDeriv.get(layerInd).get(oldLayerInd).get(i).get(j) +
                                        getDeriv().get(layerInd).get(iIter).get(jIter) *
                                        paddedLayers.get(oldLayerInd).get(iSt + i).get(jSt + j));
                                }
                            }
                        }
                    }
                }
            }
            prev.addDeriv(pushDeriv.map(layer -> {
                addPaddedDeriv(layer);
                List<List<Double>> derivLayer = new ArrayList<>();
                for (int i = 0; i < oldDim; i++) {
                    List<Double> row = new ArrayList<>();
                    for (int j = 0; j < oldDim; j++) {
                        row.add(layer.get(i + p).get(j + p));
                    }
                    derivLayer.add(row);
                }
                return new SquareMatrix(derivLayer);
            }));
        }

        @Override
        public void printParamDeriv() {
            for (Matrix3D old : getKernelDeriv()) {
                for (SquareMatrix new_ : old.getLayers()) {
                    for (List<Double> row : new_.getSource()) {
                        for (double val : row) {
                            System.out.print(val + " ");
                        }
                    }
                }
            }
            System.out.println();
        }

        private List<Matrix3D> getKernelDeriv() {
            return kernelDeriv;
        }

        protected abstract void addPaddedDeriv(SquareMatrix derivLayer);
    }

    static class CnvmNode extends CnvxNode {
        CnvmNode(int h, int k, int s, int p, List<Double> unparsedKernel, Node prev) {
            super(h, k, s, p, unparsedKernel, prev);
        }

        @Override
        protected void fill(int dim, SquareMatrix matrix) {
            for (int i = 0; i < p; i++) {
                for (int j = 0; j < p; j++) {
                    matrix.get(i).set(j, matrix.get(2 * p - i).get(2 * p - j));
                }
                for (int j = p; j < dim + p; j++) {
                    matrix.get(i).set(j, matrix.get(2 * p - i).get(j));
                }
                for (int j = dim + p; j < dim + 2 * p; j++) {
                    matrix.get(i).set(j, matrix.get(2 * p - i).get(2 * (dim + p - 1) - j));
                }
            }
            for (int i = p; i < dim + p; i++) {
                for (int j = 0; j < p; j++) {
                    matrix.get(i).set(j, matrix.get(i).get(2 * p - j));
                }
                for (int j = dim + p; j < dim + 2 * p; j++) {
                    matrix.get(i).set(j, matrix.get(i).get(2 * (dim + p - 1) - j));
                }
            }
            for (int i = dim + p; i < dim + 2 * p; i++) {
                for (int j = 0; j < p; j++) {
                    matrix.get(i).set(j, matrix.get(2 * (dim + p - 1) - i).get(2 * p - j));
                }
                for (int j = p; j < dim + p; j++) {
                    matrix.get(i).set(j, matrix.get(2 * (dim + p - 1) - i).get(j));
                }
                for (int j = dim + p; j < dim + 2 * p; j++) {
                    matrix.get(i).set(j, matrix.get(2 * (dim + p - 1) - i).get(2 * (dim + p - 1) - j));
                }
            }
        }

        @Override
        protected void addPaddedDeriv(SquareMatrix derivLayer) {
            int dim = derivLayer.dim - 2 * p;
            for (int i = 0; i < p; i++) {
                for (int j = 0; j < p; j++) {
                    derivLayer.get(2 * p - i).set(2 * p - j,
                        derivLayer.get(2 * p - i).get(2 * p - j) + derivLayer.get(i).get(j));
                }
                for (int j = p; j < dim + p; j++) {
                    derivLayer.get(2 * p - i).set(j,
                        derivLayer.get(2 * p - i).get(j) + derivLayer.get(i).get(j));
                }
                for (int j = dim + p; j < dim + 2 * p; j++) {
                    derivLayer.get(2 * p - i).set(2 * (dim + p - 1) - j,
                        derivLayer.get(2 * p - i).get(2 * (dim + p - 1) - j) + derivLayer.get(i).get(j));
                }
            }
            for (int i = p; i < dim + p; i++) {
                for (int j = 0; j < p; j++) {
                    derivLayer.get(i).set(2 * p - j,
                        derivLayer.get(i).get(2 * p - j) + derivLayer.get(i).get(j));
                }
                for (int j = dim + p; j < dim + 2 * p; j++) {
                    derivLayer.get(i).set(2 * (dim + p - 1) - j,
                        derivLayer.get(i).get(2 * (dim + p - 1) - j) + derivLayer.get(i).get(j));
                }
            }
            for (int i = dim + p; i < dim + 2 * p; i++) {
                for (int j = 0; j < p; j++) {
                    derivLayer.get(2 * (dim + p - 1) - i).set(2 * p - j,
                        derivLayer.get(2 * (dim + p - 1) - i).get(2 * p - j) + derivLayer.get(i).get(j));
                }
                for (int j = p; j < dim + p; j++) {
                    derivLayer.get(2 * (dim + p - 1) - i).set(j,
                        derivLayer.get(2 * (dim + p - 1) - i).get(j) + derivLayer.get(i).get(j));
                }
                for (int j = dim + p; j < dim + 2 * p; j++) {
                    derivLayer.get(2 * (dim + p - 1) - i).set(2 * (dim + p - 1) - j,
                        derivLayer.get(2 * (dim + p - 1) - i).get(2 * (dim + p - 1) - j) + derivLayer.get(i).get(j));
                }
            }
        }
    }

    static class CnveNode extends CnvxNode {
        CnveNode(int h, int k, int s, int p, List<Double> unparsedKernel, Node prev) {
            super(h, k, s, p, unparsedKernel, prev);
        }

        @Override
        protected void fill(int dim, SquareMatrix matrix) {
            for (int i = 0; i < p; i++) {
                for (int j = 0; j < p; j++) {
                    matrix.get(i).set(j, matrix.get(p).get(p));
                }
                for (int j = p; j < dim + p; j++) {
                    matrix.get(i).set(j, matrix.get(p).get(j));
                }
                for (int j = dim + p; j < dim + 2 * p; j++) {
                    matrix.get(i).set(j, matrix.get(p).get(p + dim - 1));
                }
            }
            for (int i = p; i < dim + p; i++) {
                for (int j = 0; j < p; j++) {
                    matrix.get(i).set(j, matrix.get(i).get(p));
                }
                for (int j = dim + p; j < dim + 2 * p; j++) {
                    matrix.get(i).set(j, matrix.get(i).get(p + dim - 1));
                }
            }
            for (int i = dim + p; i < dim + 2 * p; i++) {
                for (int j = 0; j < p; j++) {
                    matrix.get(i).set(j, matrix.get(dim + p - 1).get(p));
                }
                for (int j = p; j < dim + p; j++) {
                    matrix.get(i).set(j, matrix.get(dim + p - 1).get(j));
                }
                for (int j = dim + p; j < dim + 2 * p; j++) {
                    matrix.get(i).set(j, matrix.get(dim + p - 1).get(dim + p - 1));
                }
            }
        }

        @Override
        protected void addPaddedDeriv(SquareMatrix derivLayer) {
            int dim = derivLayer.dim - 2 * p;
            for (int i = 0; i < p; i++) {
                for (int j = 0; j < p; j++) {
                    derivLayer.get(p).set(p, derivLayer.get(p).get(p) + derivLayer.get(i).get(j));
                }
                for (int j = p; j < dim + p; j++) {
                    derivLayer.get(p).set(j, derivLayer.get(p).get(j) + derivLayer.get(i).get(j));
                }
                for (int j = dim + p; j < dim + 2 * p; j++) {
                    derivLayer.get(p).set(p + dim - 1, derivLayer.get(p).get(p + dim - 1) + derivLayer.get(i).get(j));
                }
            }
            for (int i = p; i < dim + p; i++) {
                for (int j = 0; j < p; j++) {
                    derivLayer.get(i).set(p, derivLayer.get(i).get(p) + derivLayer.get(i).get(j));
                }
                for (int j = dim + p; j < dim + 2 * p; j++) {
                    derivLayer.get(i).set(p + dim - 1, derivLayer.get(i).get(p + dim - 1) + derivLayer.get(i).get(j));
                }
            }
            for (int i = dim + p; i < dim + 2 * p; i++) {
                for (int j = 0; j < p; j++) {
                    derivLayer.get(dim + p - 1).set(p, derivLayer.get(dim + p - 1).get(p) + derivLayer.get(i).get(j));
                }
                for (int j = p; j < dim + p; j++) {
                    derivLayer.get(dim + p - 1).set(j, derivLayer.get(dim + p - 1).get(j) + derivLayer.get(i).get(j));
                }
                for (int j = dim + p; j < dim + 2 * p; j++) {
                    derivLayer.get(dim + p - 1).set(dim + p - 1,
                        derivLayer.get(dim + p - 1).get(dim + p - 1) + derivLayer.get(i).get(j));
                }
            }
        }
    }

    static class CnvcNode extends CnvxNode {
        CnvcNode(int h, int k, int s, int p, List<Double> unparsedKernel, Node prev) {
            super(h, k, s, p, unparsedKernel, prev);
        }

        @Override
        protected void fill(int dim, SquareMatrix matrix) {
            for (int i = 0; i < p; i++) {
                for (int j = 0; j < p; j++) {
                    matrix.get(i).set(j, matrix.get(dim + i).get(dim + j));
                }
                for (int j = p; j < dim + p; j++) {
                    matrix.get(i).set(j, matrix.get(dim + i).get(j));
                }
                for (int j = dim + p; j < dim + 2 * p; j++) {
                    matrix.get(i).set(j, matrix.get(dim + i).get(j - dim));
                }
            }
            for (int i = p; i < dim + p; i++) {
                for (int j = 0; j < p; j++) {
                    matrix.get(i).set(j, matrix.get(i).get(dim + j));
                }
                for (int j = dim + p; j < dim + 2 * p; j++) {
                    matrix.get(i).set(j, matrix.get(i).get(j - dim));
                }
            }
            for (int i = dim + p; i < dim + 2 * p; i++) {
                for (int j = 0; j < p; j++) {
                    matrix.get(i).set(j, matrix.get(i - dim).get(dim + j));
                }
                for (int j = p; j < dim + p; j++) {
                    matrix.get(i).set(j, matrix.get(i - dim).get(j));
                }
                for (int j = dim + p; j < dim + 2 * p; j++) {
                    matrix.get(i).set(j, matrix.get(i - dim).get(j - dim));
                }
            }
        }

        @Override
        protected void addPaddedDeriv(SquareMatrix derivLayer) {
            int dim = derivLayer.dim - 2 * p;
            for (int i = 0; i < p; i++) {
                for (int j = 0; j < p; j++) {
                    derivLayer.get(dim + i).set(dim + j, derivLayer.get(dim + i).get(dim + j) + derivLayer.get(i).get(j));
                }
                for (int j = p; j < dim + p; j++) {
                    derivLayer.get(dim + i).set(j, derivLayer.get(dim + i).get(j) + derivLayer.get(i).get(j));
                }
                for (int j = dim + p; j < dim + 2 * p; j++) {
                    derivLayer.get(dim + i).set(j - dim, derivLayer.get(dim + i).get(j - dim) + derivLayer.get(i).get(j));
                }
            }
            for (int i = p; i < dim + p; i++) {
                for (int j = 0; j < p; j++) {
                    derivLayer.get(i).set(dim + j, derivLayer.get(i).get(dim + j) + derivLayer.get(i).get(j));
                }
                for (int j = dim + p; j < dim + 2 * p; j++) {
                    derivLayer.get(i).set(j - dim, derivLayer.get(i).get(j - dim) + derivLayer.get(i).get(j));
                }
            }
            for (int i = dim + p; i < dim + 2 * p; i++) {
                for (int j = 0; j < p; j++) {
                    derivLayer.get(i - dim).set(dim + j, derivLayer.get(i - dim).get(dim + j) + derivLayer.get(i).get(j));
                }
                for (int j = p; j < dim + p; j++) {
                    derivLayer.get(i - dim).set(j, derivLayer.get(i - dim).get(j) + derivLayer.get(i).get(j));
                }
                for (int j = dim + p; j < dim + 2 * p; j++) {
                    derivLayer.get(i - dim).set(j - dim, derivLayer.get(i - dim).get(j - dim) + derivLayer.get(i).get(j));
                }
            }
        }
    }
} 