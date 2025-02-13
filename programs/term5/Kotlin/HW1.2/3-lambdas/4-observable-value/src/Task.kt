typealias obs<T> = (T) -> Unit

fun interface ObserveDeleter {
    fun cancel()
}

interface Value<T> {
    val value: T
    fun observe(observer: obs<T>): ObserveDeleter
}

class MutableValue<T>(initial: T) : Value<T> {
    private val observers: MutableSet<obs<T>> = mutableSetOf()

    override var value: T = initial
        set(newValue) {
            field = newValue
            observers.forEach { it(field) }
        }

    override fun observe(observer: obs<T>): ObserveDeleter {
        observers.add(observer)
        observer(value)
        return ObserveDeleter { observers.remove(observer) }
    }
}
