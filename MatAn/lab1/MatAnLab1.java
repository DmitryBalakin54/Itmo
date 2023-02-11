import org.knowm.xchart.*;
import org.knowm.xchart.internal.style.SeriesColorMarkerLineStyle;
import org.knowm.xchart.style.colors.SeriesColors;
import org.knowm.xchart.style.lines.SeriesLines;
import org.knowm.xchart.style.markers.SeriesMarkers;

import java.io.IOException;
import java.util.Arrays;

public class MatAnLab1 {
    static final double EPSILON = 0.1;
    static final double EPSILON2 = 0.1;
    static final int BEGIN = -20;

    public static void main(String[] args) {

        // 100 first points
        XYChart points = new XYChart(1920 / 4 * 3, 1080 / 4 * 3);
        addPoints(points, "sequence", 1, 100, 1);    // sequence
        addPoints(points, "subsequence", 4, 100, 4);   //subsequence = (2 + cos(pi*n/2) = 1, n = 4k)
        new SwingWrapper<>(points).displayChart();  // 100 first points graphic

        int end = 10;
        final int FINE_UPPER_BOUND_NUMBER = fineUpperBoundNumber(EPSILON2);
        XYChart chart = new XYChart(1920 / 2, 1080 / 2);
        // points
        addPoints(chart, "sequence", 1, 100, 1);    // sequence
        addPoints(chart, "subsequence", 4, 100, 4);   //subsequence = (2 + cos(pi*n/2) = 1, n = 4k)
        final int EPS_NUMBER = epsilonNeighborhoodNumber(EPSILON);
        addPoints(chart, "subsequence \n in epsilon \n neighborhood", EPS_NUMBER,
                EPS_NUMBER + 400 - 4, 4); // subsequence in epsilon neighborhood
        addPoint(chart, "fine upper \n bound point", FINE_UPPER_BOUND_NUMBER); //fine upper bound point (square on the graph)

        end = Math.max(FINE_UPPER_BOUND_NUMBER + 10, end);
        end = Math.max(EPS_NUMBER + 400 - 4 + 10, end);

        // lines
        addLine(chart, "sup(sequence)", BEGIN, 9, end, 9 );  // sup
        addLine(chart, "inf(sequence)", BEGIN, 1.25, end, 1.25); //inf
        addLine(chart, "upperLimit(sequence)", BEGIN, 9, end, 9);  //upperLimit
        addLine(chart, "lowerLimit(sequence)", BEGIN, 3, end, 3);  //lowerLimit
        // subsequence limit is upper limit

        new SwingWrapper<>(chart).displayChart();  // full graphic

        try {
            BitmapEncoder.saveBitmap(chart, "graphic2", BitmapEncoder.BitmapFormat.PNG);
        } catch (IOException e) {
            System.out.println("Could not save chart: " + e.getMessage());
        }

        try {
            BitmapEncoder.saveBitmap(points, "graphic1", BitmapEncoder.BitmapFormat.PNG);
        } catch (IOException e) {
            System.out.println("Could not save chart: " + e.getMessage());
        }
    }

    public static void addPoint(XYChart chart, String name, int number) {
        Graphic graphic = new Graphic();
        graphic.appendXY(number, x(number));
        XYSeries series = chart.addSeries(name, graphic.getX(), graphic.getY());
        series.setMarker(SeriesMarkers.SQUARE);
        series.setLineStyle(SeriesLines.NONE);
    }
    public static void addPoints(XYChart chart, String name, int begin, int end, int step) {
        Graphic graphic = new Graphic();
        for (int i = begin; i <= end; i += step) {
            graphic.appendXY(i, x(i));
        }
        XYSeries series = chart.addSeries(name, graphic.getX(), graphic.getY());
        series.setMarker(SeriesMarkers.CIRCLE);
        series.setLineStyle(SeriesLines.NONE);
    }

    public static void addLine(XYChart chart, String name, double leftX, double leftY, double rightX, double rightY ) {
        Graphic graphic = new Graphic();
        graphic.appendXY(leftX, leftY);
        graphic.appendXY(rightX, rightY);
        XYSeries series = chart.addSeries(name, graphic.getX(), graphic.getY());
        series.setMarker(SeriesMarkers.NONE);
        series.setLineStyle(SeriesLines.SOLID);
    }

    public static void printMinX(int begin, int end) {
        double minX = 0;
        int minInd = 0;
        for (int i = begin; i <= end; i++) {
            double x = x(i);
            if ( x < minX || minX == 0 && minInd == 0) {
                minX = x;
                minInd = i;
            }
        }
        System.out.println("min value: " + minX + " min index: " + minInd);
    }

    public static double x(int number) {
        int cosPlusTwo;
        if (number % 4 == 0) {
            cosPlusTwo = 3;
        } else if (number % 4 == 2) {
            cosPlusTwo = 1;
        } else {
            cosPlusTwo = 2;
        }
        return cosPlusTwo * ( (3 * number - 1) / (number + 2d));
    }

    public static int epsilonNeighborhoodNumber(double epsilon) {
        int number = 4;
        final int LIMIT = 9;
        while  ( !(x(number) > LIMIT - epsilon & x(number) < LIMIT + epsilon) ) {
            number += 4;
        }
        return number;
    }

    public static int fineUpperBoundNumber(double epsilon) {
        int UPPER_BOUND = 9;
        int m = 0;
        while ( x(++m) <= UPPER_BOUND - epsilon) {}
        return m;
    }

    static class Graphic {
        double[] x;
        double[] y;
        int amountOfPoints;
        Graphic() {
            x = new double[2];
            y = new double[2];
            amountOfPoints = 0;
        }

        public  void resizeArray() {
            if (x.length == amountOfPoints) {
                x = Arrays.copyOf(x, amountOfPoints * 2);
                y = Arrays.copyOf(y, amountOfPoints * 2);
            }
        }

        public void appendXY(double x, double y) {
            resizeArray();
            this.x[amountOfPoints] = x;
            this.y[amountOfPoints] = y;
            amountOfPoints++;
        }

        public double[] getX() {
            return Arrays.copyOf(x, amountOfPoints);
        }

        public double[] getY() {
            return Arrays.copyOf(y, amountOfPoints);
        }

        public int getAmountOfPoints() {
            return amountOfPoints;
        }
    }
}
