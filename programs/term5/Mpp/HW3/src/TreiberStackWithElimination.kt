import java.util.concurrent.*
import java.util.concurrent.atomic.*

/**
 * @author TODO: Balakin Dmitry
 */
open class TreiberStackWithElimination<E> : Stack<E> {
    private val stack = TreiberStack<E>()

    private val eliminationArray = AtomicReferenceArray<Any?>(ELIMINATION_ARRAY_SIZE)

    override fun push(element: E) {
        if (tryPushElimination(element)) return
        stack.push(element)
    }

    protected open fun tryPushElimination(element: E): Boolean {
        val index = randomCellIndex()
        
        if (eliminationArray.get(index) == CELL_STATE_EMPTY) {
            if (eliminationArray.compareAndSet(index, CELL_STATE_EMPTY, element)) {
                repeat(ELIMINATION_WAIT_CYCLES) {
                    if (eliminationArray.get(index) == CELL_STATE_RETRIEVED) {
                        eliminationArray.set(index, CELL_STATE_EMPTY)
                        return true
                    }
                }

                return eliminationArray.getAndSet(index, CELL_STATE_EMPTY) == CELL_STATE_RETRIEVED
            }
        }
        return false
    }

    override fun pop(): E? = tryPopElimination() ?: stack.pop()

    private fun tryPopElimination(): E? {
        val index = randomCellIndex()
        val element = eliminationArray.get(index)
        if (element != CELL_STATE_EMPTY && element != CELL_STATE_RETRIEVED) {
            if (eliminationArray.compareAndSet(index, element, CELL_STATE_RETRIEVED)) {
                return element as E
            }
        }
        return null
    }

    private fun randomCellIndex(): Int =
        ThreadLocalRandom.current().nextInt(eliminationArray.length())

    companion object {
        private const val ELIMINATION_ARRAY_SIZE = 2
        private const val ELIMINATION_WAIT_CYCLES = 1

        private val CELL_STATE_EMPTY = null

        private val CELL_STATE_RETRIEVED = Any()
    }
}
