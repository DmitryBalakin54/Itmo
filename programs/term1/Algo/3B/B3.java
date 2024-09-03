import java.util.Scanner;

public class B3 {
    public static void main(String[] args) {
        mainProgram();
    }

    public static void mainProgram() {
        Scanner scan = new Scanner(System.in);
        int n = scan.nextInt();
        int m = scan.nextInt();
        int[] array = new int[n];

        for (int i = 0; i < n; i++) {
            array[i] = scan.nextInt();
        }

        for (int i = 0; i < m; i++) {
            int value = scan.nextInt();
            int index = binSearch(array, value);
            if (index == -1) {
                System.out.println(0);
                continue;
            }
            int leftIndex = index;
            int rightIndex = index;

            while (true) {
                int nextLeftIndex = binSearch(array, value, 0, leftIndex);
                if (leftIndex == nextLeftIndex) {
                    if (leftIndex > 0) {
                        if (array[leftIndex - 1] == value) {
                            nextLeftIndex--;
                        } else {
                            break;
                        }
                    } else {
                        break;
                    }
                }
                leftIndex = nextLeftIndex;
            }

            while (true) {
                int nextRightIndex = binSearch(array, value, rightIndex, array.length - 1);
               // System.err.println(nextRightIndex + " " + rightIndex);
                if (nextRightIndex == rightIndex) {
                    if (rightIndex < array.length - 1) {
                        if (array[nextRightIndex + 1] == value) {
                            nextRightIndex++;
                        } else {
                            break;
                        }
                    } else {
                        break;
                    }
                }
                rightIndex = nextRightIndex;
            }
            System.out.println((leftIndex + 1) +  " " + (rightIndex + 1));
        }
        scan.close();
    }

    public static int binSearch(int[] array, int value) {
        return binSearch(array, value, 0, array.length - 1);
    }

    public static int binSearch(int[] array, int value, int left, int right) {
        int index = (left + right) / 2;
        if (left == right && array[left] == value) {
            return left;
        }
        while (left < right) {
            if (array[index] > value) {
                right = index;
            } else if (array[index] < value) {
                left = index;
            } else {
                return index;
            }
            index = (left + right) / 2;
            if (index == left || index == right) {
                if (array[left] == value) {
                    return left;
                } else  if (array[right] == value) {
                    return right;
                } else {
                        return -1;
                    }
                }
            }
        return -1;
    }
}
