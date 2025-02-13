class Listener : Publisher.Callback {
    override fun onEvent(event: String) {
        println(event)
    }
}

fun doSome(publisher: Publisher) {
    val listener = Listener()
    publisher.subscribe(listener)
}

fun doSomeMoreShortly(publisher: Publisher) {
    val listener = object : Publisher.Callback {
        override fun onEvent(event: String) {
            println(event)
        }
    }
    publisher.subscribe(listener)
}

fun doSomeShort(publisher: Publisher) {
    publisher.subscribe(Publisher.Callback { event -> println(event) })
    // или
    publisher.subscribe { println(it) }
}
