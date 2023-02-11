import org.knowm.xchart.*;

import java.io.IOException;
import java.util.*;

import static java.lang.Math.*;

public class lab2 {
    static final int n1 = 2;
    static final int n2 = 3;

    static final double a = -0.05;

    static final double DELTA1 = 0.001;
    static final double DELTA2 = 0.000001;

    static final double BEGIN = - 4 * PI / 2;
    static final double END = 4 * PI / 2;
    static final double STEP = 0.01;

    static final int WIDTH = 1280;
    static final int HEIGHT = 720;

    static  XYChart ALL_GRAPHICS = new XYChart(WIDTH, HEIGHT);

    public static void main(String[] args) {
        XYChart functionGraphic = makeFunctionGraphic(BEGIN, END, STEP);
        List<XYChart> polynomialsGraphic = makeTaylorPolynomialGraphics(BEGIN, END, STEP, 1, n2);
        new SwingWrapper(functionGraphic).displayChart();
        saveChart(functionGraphic, functionGraphic.getTitle());
        for (XYChart chart : polynomialsGraphic) {
            new SwingWrapper(chart).displayChart();
            saveChart(chart, chart.getTitle());
        }
        ALL_GRAPHICS.getStyler().setMarkerSize(1);
        ALL_GRAPHICS.setTitle("All graphics");
        saveChart(ALL_GRAPHICS, ALL_GRAPHICS.getTitle());
        new SwingWrapper(ALL_GRAPHICS).displayChart();

        double function = function(a);
        double n1Function = taylorPolynomial(a, n1);
        double n2Function = taylorPolynomial(a, n2);
        System.out.println("Taylor polynomial of " + n1 + " order = " + n1Function);
        if (abs(function-n1Function) < DELTA1) {
            System.out.println("Error of Taylor polynomial of " + n1 + " order less than DELTA1: "
            + abs(function - n1Function) + " < " + DELTA1);
        } else {
            System.out.println("Error of Taylor polynomial of " + n1 + " order more than DELTA1: "
                    + abs(function - n1Function) + " > " + DELTA1);
        }
        System.out.println();
        System.out.println("Taylor polynomial of " + n2 + " order = " + n2Function);
        if (abs(function-n2Function) < DELTA2) {
            System.out.println("Error of Taylor polynomial of " + n2 + " order less than DELTA2: "
                    + abs(function - n2Function) + " < " + DELTA2);
        } else {
            System.out.println("Error of Taylor polynomial of " + n2 + " order more than DELTA2: "
                    + abs(function - n2Function) + " > " + DELTA2);
        }


    }

    public static void saveChart(XYChart chart, String name) {
        try {
            BitmapEncoder.saveBitmap(chart, name, BitmapEncoder.BitmapFormat.PNG);
        } catch (IOException e) {
            System.out.println("Could not save chart: " + e.getMessage());
        }
    }
    public static List<XYChart> makeTaylorPolynomialGraphics(double begin, double end, double step, int beginOrder, int endOrder) {
        int amount = 0;
        for (double value = begin; value <= end; value += step) {
            amount++;
        }

        List<XYChart> list = new ArrayList<>();
        for (int i = beginOrder; i <= endOrder; i++) {
            double[] x = new double[amount];
            double[] y = new double[amount];

            int counter = 0;
            for (double value = begin; value <= end; value += step) {
                x[counter] = value;
                y[counter] = taylorPolynomial(value, i);
                counter++;
            }

            XYChart chart = new XYChart(WIDTH, HEIGHT);
            chart.addSeries( "cos(x + PI / 4)",
                    x,
                    y);
            chart.getStyler().setMarkerSize(1);
            chart.setTitle("Taylor polynomial of " + i + " order from " + (float) x[0] + " to " + (float) x[x.length - 1]);
            list.add(chart);
            ALL_GRAPHICS.addSeries("Taylor polynomial of " + i + " order", x, y);
        }
        return list;
    }
    public static XYChart makeFunctionGraphic(double begin, double end, double step) {
        int amount = 0;
        for (double value = begin; value <= end; value += step) {
           amount++;
        }

        double[] x = new double[amount];
        double[] y = new double[amount];

        int counter = 0;
        for (double value = begin; value <= end; value += step) {
            x[counter] = value;
            y[counter] = function(value);
            counter++;
        }

        ALL_GRAPHICS.addSeries("Function", x, y);

        XYChart chart = new XYChart(WIDTH, HEIGHT);
        chart.addSeries( "cos(x + PI / 4)",
                x,
                y);
        chart.getStyler().setMarkerSize(1);
        chart.setTitle("Function from" + (float) x[0] + " to " + (float )x[x.length - 1]);
        return chart;
    }
    public static double function(double x) {
        return cos(x + PI / 4);
    }

    public static double functionDerivative(double x, int order) {
        if (order % 4 == 0) {
            return cos(x + PI / 4);
        } else if (order % 4 == 1) {
            return - sin(x + PI / 4);
        } else if (order % 4 == 2) {
            return - cos(x + PI / 4);
        } else {
            return sin(x + PI / 4);
        }
    }

    public static double taylorPolynomial(double x, int order) {
        double result = 0;
        for (int i = 0; i <= order; i++) {
            result += (functionDerivative(0, i) / fact(i))* positivePower(x, i);
        }
        return result;
    }

    public static long fact(int n) {
        if (n == 0) {
            return 1;
        }

        return n * fact(n - 1);
    }

    public static double positivePower(double x, int n) {
        if (n == 0) {
            return 1;
        }

        double result = 1;
        for (int i = 0; i < n; i++) {
            result *= x;
        }
        return result;
    }
}
