import java.util.concurrent.atomic.AtomicReference

/**
 * @author TODO: Balakin Dmitry
 */
class TreiberStack<E> : Stack<E> {
    private val top = AtomicReference<Node<E>?>(null)

    override fun push(element: E) {
        while (true) {
            val top = top.get()
            val node = Node(element, top)
            if (this.top.compareAndSet(top, node)) {
                return
            }
        }
    }

    override fun pop(): E? {
        while (true) {
            val top = top.get() ?: return null
            if (this.top.compareAndSet(top, top.next)) {
                return top.element
            }
        }
    }

    private class Node<E>(
        val element: E,
        val next: Node<E>?
    )
}
