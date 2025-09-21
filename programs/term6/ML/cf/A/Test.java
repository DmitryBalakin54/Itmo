import java.util.*;

public class Test {

    static int activation(double y) {
        if (y > 0) return 1;
        if (y < 0) return 0;
        throw new RuntimeException("Ошибка: Y = 0 (активация не определена)");
    }

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);

        int D = sc.nextInt(); // число слоев
        int[] n = new int[D];
        for (int i = 0; i < D; i++) {
            n[i] = sc.nextInt();
        }
        // число входов n_0 = M
        int M = 0;
        if (D > 0) {
            M = sc.hasNextInt() ? n[0] : 0; // число входов
            // Можем сразу взять M = n_0 по условию, но n_0 в выводе не указано явно.
            // По условию n_0 = M, но нам нужно вывести именно M
        }

        // Чтобы узнать M, нам нужно из условия: n_0 = M.
        // n_0 не выводится, но в весах первого слоя каждый нейрон имеет n_0 входов.
        // Посчитаем размер весов первого нейрона первого слоя, чтобы узнать M

        // Считаем веса и bias для каждого слоя:
        // Для слоя i: n[i] нейронов, у каждого n_{i-1} весов + 1 bias
        // Но n_0 неизвестно, возьмём по весам первого слоя

        // Сохраним веса и смещения
        // Веса: layersWeights[layer][neuron][weight]
        // Смещения: layersBiases[layer][neuron]

        // Сначала считать веса первого нейрона первого слоя, чтобы узнать M
        // Потом считать все данные.

        // Читаем веса для первого слоя построчно, пока не достигнем n[0] нейронов

        // Чтобы упростить, прочитаем все данные в память.

        // Считаем n[0] нейронов первого слоя, чтобы узнать число весов (= M)

        // Сделаем так:

        // Считаем первый нейрон первого слоя - сколько весов он содержит?
        // Читаем первую строк с весами

        // Так как веса и bias - вещественные, считываем как строки, потом разбиваем

        sc.nextLine(); // перейти на новую строку после n_i

        // Считаем веса и bias для первого слоя
        List<String> firstLayerLines = new ArrayList<>();
        for (int i = 0; i < n[0]; i++) {
            firstLayerLines.add(sc.nextLine());
        }
        // Разберём первый нейрон первого слоя
        String[] parts = firstLayerLines.get(0).split("\\s+");
        int Mcalc = parts.length - 1; // весов, без bias

        // Теперь знаем M = Mcalc
        M = Mcalc;

        // Сохраним веса первого слоя
        double[][][] layersWeights = new double[D][][];
        double[][] layersBiases = new double[D][];

        layersWeights[0] = new double[n[0]][M];
        layersBiases[0] = new double[n[0]];
        for (int i = 0; i < n[0]; i++) {
            String[] p = firstLayerLines.get(i).trim().split("\\s+");
            for (int j = 0; j < M; j++) {
                layersWeights[0][i][j] = Double.parseDouble(p[j]);
            }
            layersBiases[0][i] = Double.parseDouble(p[M]);
        }

        // Теперь считаем остальные слои
        for (int layer = 1; layer < D; layer++) {
            layersWeights[layer] = new double[n[layer]][n[layer - 1]];
            layersBiases[layer] = new double[n[layer]];
            for (int i = 0; i < n[layer]; i++) {
                String line = sc.nextLine();
                String[] p = line.trim().split("\\s+");
                for (int j = 0; j < n[layer - 1]; j++) {
                    layersWeights[layer][i][j] = Double.parseDouble(p[j]);
                }
                layersBiases[layer][i] = Double.parseDouble(p[n[layer - 1]]);
            }
        }

        // Теперь перебираем все входы длины M и считаем выход сети

        int totalInputs = 1 << M;

        for (int mask = 0; mask < totalInputs; mask++) {
            double[] input = new double[M];
            for (int bit = 0; bit < M; bit++) {
                input[M - 1 - bit] = ((mask >> bit) & 1);
            }

            double[] currentLayerOutput = input;

            for (int layer = 0; layer < D; layer++) {
                double[] nextLayerOutput = new double[n[layer]];
                for (int neuron = 0; neuron < n[layer]; neuron++) {
                    double sum = layersBiases[layer][neuron];
                    for (int w = 0; w < layersWeights[layer][neuron].length; w++) {
                        sum += layersWeights[layer][neuron][w] * currentLayerOutput[w];
                    }
                    int a = activation(sum);
                    nextLayerOutput[neuron] = a;
                }
                currentLayerOutput = nextLayerOutput;
            }

            // Выводим вход и результат
            System.out.print("Input: ");
            for (int i = 0; i < M; i++) {
                System.out.print((int)input[i] + " ");
            }
            System.out.println("-> Output: " + (int)currentLayerOutput[0]);
        }
    }
}
