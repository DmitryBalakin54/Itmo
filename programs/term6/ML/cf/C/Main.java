import java.util.Scanner;

public class Main {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        int n = scanner.nextInt();
        int m = scanner.nextInt();

        double[][] I = new double[n][n];
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                I[i][j] = scanner.nextDouble();
            }
        }

        double[][] O = new double[m][m];
        for (int i = 0; i < m; i++) {
            for (int j = 0; j < m; j++) {
                O[i][j] = scanner.nextDouble();
            }
        }

        int k = n - m + 1;

        double[][] A = new double[m * m][k * k];
        double[] O_vec = new double[m * m];

        for (int i = 0; i < m; i++) {
            for (int j = 0; j < m; j++) {
                O_vec[i * m + j] = O[i][j];
                for (int p = 0; p < k; p++) {
                    for (int q = 0; q < k; q++) {
                        A[i * m + j][p * k + q] = I[i + p][j + q];
                    }
                }
            }
        }

        double[] K_vec = leastSquares(A, O_vec);

        double[][] K = new double[k][k];
        for (int i = 0; i < k; i++) {
            for (int j = 0; j < k; j++) {
                K[i][j] = K_vec[i * k + j];
            }
        }

        for (int i = 0; i < k; i++) {
            for (int j = 0; j < k; j++) {
                System.out.printf("%.10f", K[i][j]);
                if (j < k - 1) {
                    System.out.print(" ");
                }
            }
            System.out.println();
        }
    }

    private static double[] leastSquares(double[][] A, double[] b) {
        int rows = A.length;
        int cols = A[0].length;

        double[][] At = new double[cols][rows];
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                At[j][i] = A[i][j];
            }
        }

        double[][] AtA = new double[cols][cols];
        for (int i = 0; i < cols; i++) {
            for (int j = 0; j < cols; j++) {
                for (int k = 0; k < rows; k++) {
                    AtA[i][j] += At[i][k] * A[k][j];
                }
            }
        }

        double[] Atb = new double[cols];
        for (int i = 0; i < cols; i++) {
            for (int j = 0; j < rows; j++) {
                Atb[i] += At[i][j] * b[j];
            }
        }

        return solve(AtA, Atb);
    }

    private static double[] solve(double[][] A, double[] b) {
        int n = b.length;

        for (int p = 0; p < n; p++) {
            int max = p;
            for (int i = p + 1; i < n; i++) {
                if (Math.abs(A[i][p]) > Math.abs(A[max][p])) {
                    max = i;
                }
            }
            double[] temp = A[p];
            A[p] = A[max];
            A[max] = temp;
            double t = b[p];
            b[p] = b[max];
            b[max] = t;

            for (int i = p + 1; i < n; i++) {
                double alpha = A[i][p] / A[p][p];
                b[i] -= alpha * b[p];
                for (int j = p; j < n; j++) {
                    A[i][j] -= alpha * A[p][j];
                }
            }
        }

        double[] x = new double[n];
        for (int i = n - 1; i >= 0; i--) {
            double sum = 0.0;
            for (int j = i + 1; j < n; j++) {
                sum += A[i][j] * x[j];
            }
            x[i] = (b[i] - sum) / A[i][i];
        }
        return x;
    }
}