import java.util.concurrent.atomic.AtomicReference

/**
 * @author Balakin Dmitry
 */
class Solution(private val env: Environment) : Lock<Solution.Node> {

    private val tail = AtomicReference<Node?>(null)

    override fun lock(): Node {
        val my = Node()
        val pred = tail.getAndSet(my)

        if (pred != null) {
            my.locked.value = true
            pred.next.value = my

            while (my.locked.value) {
                env.park()
            }
        }

        return my
    }

    override fun unlock(node: Node) {
        val next = node.next.value
        if (next == null) {
            if (tail.compareAndSet(node, null)) {
                return
            }

            while (node.next.value == null) {}
        }

        node.next.value?.let {
            it.locked.value = false
            env.unpark(it.thread)
        }
    }

    class Node {
        val thread: Thread = Thread.currentThread()
        val locked = AtomicReference(false)
        val next = AtomicReference<Node?>(null)
    }
}
