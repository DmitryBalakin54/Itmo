import java.util.*;

public class A {
    public static void main(String[] args) {
        mainProgram();
    }

    public static void  mainProgram() {
        Scanner scan = new Scanner(System.in);
        int n = scan.nextInt();
        List<Element> list = new ArrayList<>();
        long[][] array = new long[2][n];
        for (int i = 0; i < n; i++) {
            array[0][i] = scan.nextInt();
            array[1][i] = 0;
            list.add(new Element(true, array[0][i], i, null));
        }
        scan.close();
        while (list.size() > 1) {
            int[] mins = searchMins(list);
            List<Long> child = new ArrayList<>();
            for (int i : mins) {
                if (!list.get(i).isLeaf) {
                    child.addAll(list.get(i).child);
                } else {
                    child.add((long)list.get(i).number);
                }
            }
            for (Long i : child) {
                array[1][Math.toIntExact(i)]++;
            }
            long value1 = list.get(mins[0]).value;
            long value2 = list.get(mins[1]).value;
            list.remove(Math.max(mins[0], mins[1]));
            list.remove(Math.min(mins[1], mins[0]));
            list.add(new Element( false, value1 + value2, -1, child));
        }
        long sum = 0;
        for (int i = 0; i < n; i++) {
            sum += (long) (array[0][i] * array[1][i]);
        }
        System.out.println(sum);
    }

    public static int[] searchMins(List<Element> list) {
        int min1 = 0;
        int min2 = 0;
        long min1Value = 0;
        long min2Value = 0;
        int i = 0;
        for (Element element : list) {
            if (min1Value == 0) {
                if (min2Value == 0) {
                    min2 = i;
                    min2Value = element.value;
                } else {
                    min1Value = element.value;
                    min1 = i;
                }
            } else if (min1Value >= element.value) {
                min2 = min1;
                min2Value = min1Value;
                min1 = i;
                min1Value = element.value;
            }
            i++;
        }
        return new int[] {min1, min2};
    }
    public  static class Element {
        boolean isLeaf;
        List<Long> child;
        long value;
        int number;

        Element(boolean isLeaf, long value, int number, List<Long> child) {
            this.isLeaf = isLeaf;
            this.value = value;
            this.child = child;
            this.number = number;
        }
    }
}
