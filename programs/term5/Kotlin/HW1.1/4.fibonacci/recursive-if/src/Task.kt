fun fibonacciIf(n: Int): Int {
    return if (n < 2) {
        n
    } else {
        fibonacciIf(n - 1) + fibonacciIf(n - 2)
    }
}
