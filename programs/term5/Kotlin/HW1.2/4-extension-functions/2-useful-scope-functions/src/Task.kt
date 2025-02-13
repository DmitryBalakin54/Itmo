fun task1(value: A?, default: B, processor: (A) -> B): B {
    return value?.let { processor(it) } ?: default
}

fun task2(value: A, modifier: (A) -> A, biFunction: (A, String) -> Long): A {
    return modifier(value).also { biFunction(it, "additional") }
}

fun task3(a: A, constructor: (A) -> C, combine: C.(compress: Boolean) -> B): B {
    return constructor(a).apply {
        setupNumber(28)
        setupText("Hello")
    }.combine(true)
}
