fun doSome(callable: () -> Unit) {}

fun interface Callable {
    fun call()
}

fun doSomeCallable(callable: Callable) {}

fun main() {
    doSome {
        println("Hello!")
    }
    doSomeCallable(callable = Callable { println("Hello!") })
    doSomeCallable { println("Hello!") }
}
