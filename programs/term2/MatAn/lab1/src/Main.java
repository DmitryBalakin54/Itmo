package src;

import org.knowm.xchart.*;

import java.io.IOException;


public class Main {

    static final double STEP = 0.01;
    static final int WIDTH = 1280;
    static final int HEIGHT = 720;
    static final boolean LOG = true;
    static final boolean SAVE = true;


    public static void main(String[] args) {
        Function f =  new MyFunction();
        Dot mid = new MyDotMid();
        Dot left = new MyDotLeft();
        Dot right = new MyDotRight();

        XYChart graphic;
        if (LOG || SAVE) {
            graphic = makeFunctionGraphic(-1, 2, STEP, f);
        }
        if (LOG) {
            new SwingWrapper(graphic).displayChart();
        }
        if (SAVE) {
            saveChart(graphic, f.getName());
        }
        printIntegralSum(f, -1, 2, 10, mid);
        printIntegralSum(f, -1, 2, 100, mid);
        printIntegralSum(f, -1, 2, 10, left);
        printIntegralSum(f, -1, 2, 100, left);
        printIntegralSum(f, -1, 2, 10, right);
        printIntegralSum(f, -1, 2, 100, right);
    }

    public static void printIntegralSum(Function f, double left, double right, int n, Dot dot) {
        System.out.println("Integral sum for " + f.getName() + " with equipment " + dot.equipment() + " and " + n + " segments is: " + integralSum(f, -1, 2, n, dot));
    }


    static double integralSum(Function f, double left, double right, int n, Dot dots) {
        double result = 0;
        XYChart graphic;
        double[] x;
        double[] y;
        if (LOG || SAVE)  {
            graphic = makeFunctionGraphic(left, right, STEP, f);
            x = new double[n * 5];
            y = new double[n * 5];
        }

        final double SEGMENT_LENGTH = (right - left)  / n;
        int begin = 0;
        while (left  < right) {
            double height = f.evaluate(dots.getDotOnSegment(left, left + SEGMENT_LENGTH));
            result += height  * SEGMENT_LENGTH;
            if (LOG || SAVE) {
                if (begin < x.length) {
                    addRect(x, y, begin, left, left + SEGMENT_LENGTH, height);
                }
                begin += 5;
            }
            left += SEGMENT_LENGTH;
        }

        if (LOG || SAVE) {
            addRects(graphic, x, y, "Integral sum for " + f.getName() + "\n(" +  n + " segments)\n equipment: " + dots.equipment());
        }

        if (LOG) {
            new SwingWrapper(graphic).displayChart();
        }

        if (SAVE) {
            saveChart(graphic, f.getName() + "(" + n + " segments, equipment " + dots.equipment() + ")");
        }

        return result;
    }

    static void saveChart(XYChart chart, String name) {
        try {
            BitmapEncoder.saveBitmap(chart, name, BitmapEncoder.BitmapFormat.PNG);
        } catch (IOException e) {
            System.out.println("Could not save chart: " + e.getMessage());
        }
    }

    private static void addRect(double[] x, double[] y, int begin, double leftX, double rightX, double height) {
        final double ZERO = 0;
        x[begin] = leftX;
        x[begin + 1] = rightX;
        x[begin + 2] = rightX;
        x[begin + 3] = leftX;
        x[begin + 4] = x[begin];

        y[begin] = ZERO;
        y[begin + 1] = ZERO;
        y[begin + 2] = height;
        y[begin + 3] = height;
        y[begin + 4] = y[begin];
    }

    private static void addRects(XYChart chart, double[] x, double[] y, String name) {
        chart.addSeries(name,x, y);
    }
    static XYChart makeFunctionGraphic(double begin, double end, double step, Function f) {
        int amount = 0;
        for (double value = begin; value <= end; value += step) {
            amount++;
        }

        double[] x = new double[amount];
        double[] y = new double[amount];

        int counter = 0;
        for (double value = begin; value <= end; value += step) {
            x[counter] = value;
            y[counter] = f.evaluate(value);
            counter++;
        }

        XYChart chart = new XYChart(WIDTH, HEIGHT);
        chart.addSeries( f.getName(), x, y);
        chart.getStyler().setMarkerSize(1);
        return chart;
    }

}

class MyFunction implements Function {

    final private String name =  "3x - 2x^2";

    public String getName() {
        return name;
    }

    public double evaluate(double x) {
        return 3 * x - 2 * x * x;
    }
}

class MyDotMid implements Dot {
    @Override
    public String equipment() {
        return "mid";
    }

    public double getDotOnSegment(double left, double right) {
        return (right + left) / 2;
    }
}

class MyDotLeft implements Dot {
    @Override
    public String equipment() {
        return "left";
    }

    public double getDotOnSegment(double left, double right) {
        return left;
    }
}

class MyDotRight implements Dot {
    @Override
    public String equipment() {
        return "right";
    }

    public double getDotOnSegment(double left, double right) {
        return right;
    }
}
interface Dot {
    String equipment();
    double getDotOnSegment(double left, double right);
}
interface Function {
    String getName();
    double evaluate(double x);
}
