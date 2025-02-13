import java.util.concurrent.atomic.*

/**
 * @author TODO: Balakin Dmitry
 */
class MSQueue<E> : Queue<E> {
    private val head: AtomicReference<Node<E>>
    private val tail: AtomicReference<Node<E>>

    init {
        val dummy = Node<E>(null)
        head = AtomicReference(dummy)
        tail = AtomicReference(dummy)
    }

    override fun enqueue(element: E) {
        val node = Node(element)

        while (true) {
            val tail = this.tail.get()

            if (tail.next.compareAndSet(null, node)) {
                this.tail.compareAndSet(tail, node)
                return
            }

            this.tail.compareAndSet(tail, tail.next.get())
        }
    }

    override fun dequeue(): E? {
        while (true) {
            val head = this.head.get()
            val tail = this.tail.get()
            val newHead = this.head.get().next.get()

            if (head == tail || newHead == null) {
                return null
            }

            if (this.head.compareAndSet(head, newHead)) {
                val el = newHead.element
                newHead.element = null
                return el
            }
        }
    }

    // FOR TEST PURPOSE, DO NOT CHANGE IT.
    override fun validate() {
        check(tail.get().next.get() == null) {
            "At the end of the execution, `tail.next` must be `null`"
        }
        check(head.get().element == null) {
            "At the end of the execution, the dummy node shouldn't store an element"
        }
    }

    private class Node<E>(
        var element: E?
    ) {
        val next = AtomicReference<Node<E>?>(null)
    }
}
