package search;


public class BinarySearch {
    // Pred: args[i] -> int && (int)args[args.length - 1] <= (int)args[0] && forall l, j from [1; args.length - 1] where l < j : (int)args[l] >= (int)args[j]
    // Post: out i == min(I)  ( I = { i' | (int)args[i'] <= (int)args[0]})
    public static void main(String[] args) {
        // Pred
        int x = Integer.parseInt(args[0]);
        // Pred && x == (int)args[0]
        int[] a = new int[args.length - 1];
        // Pred && x == (int)args[0] && forall i from a : i == 0
        for (int i = 1; i < args.length; i++) {
            a[i - 1] = Integer.parseInt(args[i]);
        }
        // Pred && x == (int)args[0] && a == (int)args[1 ... args.length - 1]
        //int res = iterateBinarySearch(a, x);
        int res = recurrentBinarySearch(a, x, -1, a.length);
        // Pred && x == (int)args[0] && a == (int)args[1 ... args.length - 1] && res == i
        System.out.println(res);
        // out i
    }

    // Pred1: forall l, j from [0; a.length - 1] where l < j : a[l] >= a[j] && exist k from [0; a.length - 1] : a[k] <= x
    // Post1: i == min(I)  ( I = { i' | (a[i'] <= x})
    public static int iterateBinarySearch(int[] a, int x) {
        // Pred1
        int left = -1;
        // Pred1 && left == -1
        int right = a.length;
        // Pred1 && left == -1 && right == a.length - 1
        // Inv: -1 <= left < right <= a.length && a[left] > x >= a[right]
        while (left + 1 < right) {
            // Inv && left + 1 < right
            int mid = (right + left) / 2;
            // Inv && left + 1 < right && left < mid < right (left + 1 < right => left / 2 + left / 2 < right / 2 + left / 2 < right / 2 + right / 2)
            if (a[mid] <= x) {
                // Inv && left + 1 < right && left < mid < right && a[mid] <= x
                right = mid;
                // Inv && left + 1 < right && left < mid < right && a[mid] <= x && left' == left && right' == mid
                // check Inv : -1 <= left' < mid (< right) <= a.length && a[left'] > x >= a[mid]
            } else {
                // Inv && left + 1 < right && left < mid < right && a[mid] > x
                left  = mid;
                // Inv && left + 1 < right && left < mid < right && a[mid] > x && left' == mid && right' == right
                // check Inv : -1 <= (left <) mid < right' <= a.length && a[mid] > x >= a[right']
            }
            // Inv && right' - left' < right - left
        }
        // Inv && right <= left + 1 (right <= left + 1 && -1 <= left < right <= a.length => left + 1 == right) => a[left] > x >= a[left + 1 == right]
        return right;
    }

    // Pred2: forall l, j from (left; right] where l < j : a[l] >= a[j] && exist k from (left; right] : a[k] <= x && left < right
    // Post2: a[left] > x >= a[left + 1] <=> left + 1 == i
    public static int recurrentBinarySearch(int[] a, int x, int left, int right) {
        // Pred2
        if (left + 1 == right) {
            // Pred2 && right == left + 1
            // check Post2: a[left] >= a[left + 1] && (left; left + 1] <=> {left + 1}  => k in {left + 1} <=> k == left + 1 == right
            return right;
        }
        // Pred2 && left + 1 != right
        int mid = (left + right) / 2;
        // Pred2 && left + 1 != right && left < mid < right (left + 1 < right => left / 2 + left / 2 < right / 2 + left / 2 < right / 2 + right / 2)
        if (a[mid] <= x) {
            // Pred2 && left + 1 != right && left < mid < right && a[mid] <= x
            // check Pred2: forall l, j from (left, mid] where l < j : a[l] >= a[j] && exists k from (left; mid] : a[k] <= x (mid) && left < mid
            return recurrentBinarySearch(a, x, left, mid);
        } else {
            // Pred2 && left + 1 != right && left < mid < right && a[mid] > x
            // check Pred2: forall l, j from (mid, right] where l < j : a[l] >= a[j] && exists k from (mid; right] : a[k] <= x (a[mid] > x && Pred2 => k in [mid + 1; right]) && mid < right
            return recurrentBinarySearch(a, x, mid, right);
        }
    }
}
