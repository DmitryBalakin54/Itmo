package search;


public class BinarySearchMax {
    // Pred: args[i] -> int &&  exists k : b == (int)args >>> k && forall l, j from [0; args.length - 1] where l < j : b[l] <= b[j] (if a[left] > a[left + t] => max(a) in [left; left + t] because b is sorted && b = a >>> k)
    // Post: out i == max((int)args) && if(sum((int)args % 2 == 0): use recurrent else: use iterate
    public static void main(String[] args) {
        // Pred
        int[] a = new int[args.length];
        // Pred && forall i from a : i == 0 && a.length == args.length
        int count = 0;
        // Pred && forall i from a : i == 0 && count == 0
        for (int i = 0; i < args.length; i++) {
            a[i] = Integer.parseInt(args[i]);
            // Pred && count + a[i] == (sum(a) - sum(a[i + 1 .. a.length])) % 2
            count = (count + a[i]) % 2;
            // Pred && count == (sum(a) - sum(a[i + 1 .. a.length])) % 2
        }
        // Pred && count == sum(a) % 2 && a == (int)args[0 ... args.length - 1]
        int res = 0;
        // Pred && count == sum(a) % 2 && a == (int)args[0 ... args.length - 1]
        if (count == 0) {
            // Pred && count == sum(a) % 2 == 0 && a == (int)args[0 ... args.length - 1]
            res = recurrentBinarySearchMax(a, 0, a.length - 1);
            // Pred && count == sum(a) % 2 == 0 && a == (int)args[0 ... args.length - 1] && res = i
        } else {
            // Pred && count == sum(a) % 2 != 0 && a == (int)args[0 ... args.length - 1]
            res = iterateBinarySearchMax(a);
            // Pred && count == sum(a) % 2 != 0 && a == (int)args[0 ... args.length - 1] && res = i
        }
        // res == i
        System.out.println(res);
    }

    // Pred2: exists k : b == a >>> k && forall l, j from [left; right] where l < j : b[l] <= b[j] && left < right && left <= ind(max(a)) <= right (if a[left] > a[left + t] => max(a) in [left; left + t] because b is sorted && b = a >>> k)
    // Post2: i == max(a)
    private static int recurrentBinarySearchMax(int[] a, int left, int right) {
            // Pred2
            if (left + 1 >= right) {
                // Pred2 && right <= left + 1 => left + 1 == right
                if (a[left] < a[right]) {
                    // Pred2 && right == left + 1 && a[left] < a[right]
                    left = right;
                    // left' == right && a[left] < a[left'] => a[left'] = max(a)
                }
                // a[left] == max(a)
                return a[left];
            }
            // Pred && left + 1 < right
            int mid = (left + right) / 2;
            // Pred2 && left + 1 < right && left < mid < right (left + 1 < right => left / 2 + left / 2 < right / 2 + left / 2 < right / 2 + right / 2)
            if (a[mid] < a[left]) {
                // Pred2 && left + 1 < right && left < mid < right && a[mid] < a[left]
                // check Pred2 : exists k : b = a >>> k forall l, j from [left; mid] where l < j : b[l] <= b[j] && left < mid && left <= ind(max(a)) <= mid
                return recurrentBinarySearchMax(a, left, mid);
            } else {
                // Pred2 && left + 1 < right && left < mid < right && a[mid] >= a[left]
                // check Pred2 : exists k : b = a >>> k forall l, j from [mid; right] where l < j : b[l] <= b[j] && mid < right && mid <= ind(max(a)) <= right
                return recurrentBinarySearchMax(a, mid, right);
            }
    }

    // Pred1: exists k : b = a >>> k && forall l, j from [0; a.length - 1] where l < j : b[l] <= b[j] &&
    // Post1: i == max(a)
    public static int iterateBinarySearchMax(int[] a) {
        //Pred1
        int left = 0;
        // Pred1 && left == 0
        int right = a.length - 1;
        // Pred1 && left == 0 && right == a.length - 1
        // Inv: 0 <= left < right <= a.length - 1 && left <= ind(max(a)) <= right
        while (left + 1 < right) {
            // Inv && left + 1 < right
            int mid = (right + left) / 2;
            // Inv && left + 1 < right && left < mid < right (left + 1 < right => left / 2 + left / 2 < right / 2 + left / 2 < right / 2 + right / 2)
            if (a[mid] < a[left] ) {
                // Inv && left + 1 < right && left < mid < right && a[mid] < a[left]
                right = mid;
                // Inv && left + 1 < right && left < mid < right && a[mid] < a[left] && left' == left && right' == mid
                // check Inv : 0 <= left' < mid (< right) <= a.length - 1 && left' >= ind(max(a)) >= mid
            } else {
                // Inv && left + 1 < right && left < mid < right && a[mid] > a[left]
                left  = mid;
                // Inv && left + 1 < right && left < mid < right && a[mid] >= a[left] && left' == mid && right' == right
                // check Inv : 0 <= (left <) mid < right' <= a.length - 1 && mid >= ind(max(a)) >= right'
            }
            // Inv && right' - left' < right - left
        }
        // Inv && right <= left + 1 (right <= left + 1 && 0 <= left < right <= a.length - 1 => left + 1 == right) => left >= ind(max(a)) >= left + 1 == right
        if (a[left] < a[right]) {
            // Inv && right == left + 1 && a[left] < a[right]
            left = right;
            // left' == right && a[left] < a[left'] => a[left'] = max(a)
        }
        // a[left] == max(a)
        return a[left];
    }
}
