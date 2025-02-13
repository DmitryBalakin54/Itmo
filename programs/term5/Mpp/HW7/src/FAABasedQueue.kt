import java.util.concurrent.atomic.*

/**
 * @author Balakin Dmitry
 */
class FAABasedQueue<E> : Queue<E> {
    private val head = AtomicReference(Segment(0))
    private val tail = head
    private val enqIdx = AtomicLong(0)
    private val deqIdx = AtomicLong(0)

    private fun Long.div(): Long = this / SEGMENT_SIZE

    private fun Long.mod(): Int = (this % SEGMENT_SIZE).toInt()

    override fun enqueue(element: E) {
        operation(false, element)
    }

    override fun dequeue(): E? {
        return operation(true, null)
    }

    @Suppress("UNCHECKED_CAST")
    private fun operation(isDeq: Boolean, element: E?): E? {
        while (!isDeq || deqIdx.get() < enqIdx.get()) {
            val curV = if (isDeq) head.get() else tail.get()
            val idx = if (isDeq) deqIdx.getAndIncrement() else enqIdx.getAndIncrement()
            val cells = getById(curV, curV.id, idx.div()).cells

            if (isDeq && !cells.compareAndSet(idx.mod(), null, Any())) {
                return cells.getAndSet(idx.mod(), null) as E?
            } else if (!isDeq && cells.compareAndSet(idx.mod(), null, element)) {
                return null
            }
        }

        return null
    }

    private fun getById(s: Segment, from: Long, to: Long): Segment {
        var el = s

        repeat((to - from).toInt()) {
            if (el.next.get() == null) {
                el.next.compareAndSet(null, Segment(it + from + 1))
            }

            el = el.next.get()!!
        }

        return el
    }
}

private class Segment(val id: Long) {
    val next = AtomicReference<Segment?>(null)
    val cells = AtomicReferenceArray<Any?>(SEGMENT_SIZE)
}

private const val SEGMENT_SIZE = 2