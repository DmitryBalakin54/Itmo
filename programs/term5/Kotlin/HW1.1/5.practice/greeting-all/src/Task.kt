fun greet(name: String): String {
    return "Hello, $name!"
}

fun main(args: Array<String>) {
    (if (args.isEmpty()) arrayOf(readlnOrNull() ?: "Anonymous") else args).forEach { println(greet(it)) }
}
