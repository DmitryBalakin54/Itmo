import kotlin.math.floor
import kotlin.math.sqrt

fun isPrime(n: Int): Boolean {
    for (i in 2..sqrt(n.toDouble()).toInt()) {
        if (n % i == 0) {
            return false
        }
    }

    return n > 1
}

fun piFunction(x: Double): Int {
    var cnt = 0
    for (i in 2..floor(x).toInt()) {
        if (isPrime(i)) cnt++
    }

    return cnt
}
