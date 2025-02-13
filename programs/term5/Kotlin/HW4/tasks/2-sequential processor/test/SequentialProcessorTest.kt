import java.util.*
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicIntegerArray
import kotlin.time.Duration.Companion.milliseconds
import kotlinx.coroutines.*
import org.junit.jupiter.api.*

@TestMethodOrder(MethodOrderer.OrderAnnotation::class)
class SequentialProcessorTest {
    @Test
    @Order(1)
    fun testCorrectness() {
        val processor: TaskProcessor = SequentialProcessor { x -> x }
        processor.use {
            runBlocking {
                processor.assertProcessResult("hello", "hello")
                delay(100.milliseconds)
                processor.assertProcessResult("coroutines", "coroutines")
                delay(100.milliseconds)
                processor.assertProcessResult("in", "in")
                delay(100.milliseconds)
                processor.assertProcessResult("kotlin", "kotlin")
            }
        }
    }

    @Test
    @Order(2)
    fun testParallel() {
        SequentialProcessor { x -> "Hello, $x!" }.use { processor ->
            testRunParallel(10, PARALLEL_TASK_COUNT) { i ->
                processor.assertProcessResult("Hello, unit-$i!", "unit-$i")
            }
        }
    }

    @Test
    @Order(3)
    fun testLaunchInCommonThread() {
        val threadIds: Queue<Long> = ConcurrentLinkedQueue()
        SequentialProcessor { x ->
            threadIds += Thread.currentThread().id
            "Hello, $x!"
        }.use { processor ->
            testRunParallel(10, PARALLEL_TASK_COUNT) { i ->
                processor.assertProcessResult("Hello, unit-$i!", "unit-$i")
            }
        }
        Assertions.assertTrue(threadIds.all { it == threadIds.first() })
    }

    @Test
    @Order(4)
    fun testLinearizability() {
        var counter = 0
        val order = AtomicIntegerArray(PARALLEL_TASK_COUNT)
        SequentialProcessor { x ->
            val number = counter++
            val index = x.toInt()
            order.set(index, number)
            ""
        }.use { processor ->
            testRunParallel(10, PARALLEL_TASK_COUNT) { i ->
                processor.assertProcessResult("", i.toString())
            }
        }

        Assertions.assertEquals(PARALLEL_TASK_COUNT, counter)

        val actualOrder = MutableList(PARALLEL_TASK_COUNT) { order[it] }
        actualOrder.sort()
        Assertions.assertEquals((0..<PARALLEL_TASK_COUNT).toList(), actualOrder)
    }

    @Test
    @Order(6)
    fun testResourceLeaks() {
        Thread.sleep(100)
        val wasThreads = Thread.activeCount()

        SequentialProcessor { x -> x }.use { processor ->
            runBlocking {
                processor.process("hello")
            }
        }
        Thread.sleep(100)

        Assertions.assertEquals(
            wasThreads,
            Thread.activeCount(),
            "All threads should be shutdown after Sequential processor usage (after close method)",
        )
    }

    companion object {
        private const val PARALLEL_TASK_COUNT = 100000

        private suspend fun TaskProcessor.assertProcessResult(expectedResult: String, argument: String) {
            val result = process(argument)
            Assertions.assertEquals(expectedResult, result)
        }
    }
}
