import java.util.concurrent.*
import java.util.concurrent.atomic.*
/**
 * @author Balakin Dmitry
 */
class FlatCombiningQueue<E> : Queue<E> {
    private val queue = ArrayDeque<E>()
    private val combinerLock = AtomicBoolean(false)
    private val tasksForCombiner = AtomicReferenceArray<Any?>(TASKS_FOR_COMBINER_SIZE)

    override fun enqueue(element: E) {
        operation(false, element)
    }

    override fun dequeue(): E? {
        return operation(true, null)
    }

    @Suppress("UNCHECKED_CAST")
    private fun operation(isDeq: Boolean, element: E?): E? {
        while (true) {
            val ind = randomCellIndex()

            if (tasksForCombiner.compareAndSet(ind, null, if (isDeq) Dequeue else element as E)) {
                while (true) {
                    if (combinerLock.compareAndSet(false, true)) {
                        processTasksForCombiner()
                        combinerLock.set(false)
                    }

                    val task = tasksForCombiner.get(ind)
                    if (task is Result<*>) {
                        tasksForCombiner.set(ind, null)
                        return (if(isDeq) task.value else null) as E?
                    }
                }
            }
        }
    }

    @Suppress("UNCHECKED_CAST")
    private fun processTasksForCombiner() {
        repeat(TASKS_FOR_COMBINER_SIZE) {
            val task = tasksForCombiner.get(it)
            when {
                task == Dequeue -> tasksForCombiner.set(it, Result(queue.removeFirstOrNull()))
                task != null && task !is Result<*> -> tasksForCombiner.set(it, Result(queue.add(task as E)))
            }
        }
    }

    private fun randomCellIndex(): Int =
        ThreadLocalRandom.current().nextInt(tasksForCombiner.length())
}

private const val TASKS_FOR_COMBINER_SIZE = 3


private object Dequeue

private class Result<V>(
    val value: V
)