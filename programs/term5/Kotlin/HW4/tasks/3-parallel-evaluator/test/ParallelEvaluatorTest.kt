import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicIntegerArray
import kotlin.coroutines.CoroutineContext
import kotlinx.coroutines.*
import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.MethodOrderer
import org.junit.jupiter.api.Order
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.TestMethodOrder

@TestMethodOrder(MethodOrderer.OrderAnnotation::class)
class ParallelEvaluatorTest {
    @Test
    @Order(1)
    fun testOneTask() {
        val result = AtomicBoolean(false)

        val task = Task { result.set(true) }

        testWithContext(2) { ctx ->
            ParallelEvaluator().run(task, 1, ctx)
        }

        Assertions.assertTrue(result.get())
    }

    @Test
    @Order(2)
    fun testBase() {
        val results = AtomicIntegerArray(10)

        val task = Task { i ->
            if (i == 0) {
                results.set(0, 1)
                return@Task
            }
            Thread.sleep(((5 - i) / 4 + 1) * 100L)
            results.set(i, 1)
        }

        testWithContext(5) { ctx ->
            val evaluator = ParallelEvaluator()
            evaluator.run(task, 10, ctx)
        }
        for (i in 0 until 10) {
            Assertions.assertEquals(1, results.get(i))
        }
    }

    @OptIn(DelicateCoroutinesApi::class)
    @Test
    @Order(3)
    fun testFailure() {
        val task = Task { i ->
            Thread.sleep((i % 3) * 100L)
            if (i == 5 || i == 2) {
                throw RuntimeException("")
            }
        }

        val exception = Assertions.assertThrows(TaskEvaluationException::class.java) {
            testWithContext(5) { ctx ->
                val evaluator = ParallelEvaluator()
                evaluator.run(task, 10, ctx)
            }
        }
        Assertions.assertTrue(exception.cause is RuntimeException)
    }

    // TODO: тест на то, что задачи запускаются в правильном порядке

    @Test
    @Order(4)
    fun testCancellation() {
        val results = AtomicIntegerArray(10)

        val task = Task { i ->
            if (i == 3) {
                Thread.sleep(10)
                throw RuntimeException("cancel")
            }
            Thread.sleep(200)
            results.set(i, 1)
        }

        testWithContext(2) { ctx ->
            val evaluator = ParallelEvaluator()

            val result = runCatching {
                evaluator.run(task, 10, ctx)
            }

            Assertions.assertTrue(result.isFailure)
            Assertions.assertTrue(result.exceptionOrNull() is TaskEvaluationException)
            Assertions.assertTrue(result.exceptionOrNull()?.cause is RuntimeException)
        }

        Assertions.assertEquals(1, results.get(0))
        Assertions.assertEquals(1, results.get(1))
        Assertions.assertEquals(1, results.get(2))
        Assertions.assertEquals(0, results.get(3))

        Assertions.assertEquals(0, results.get(6))
        Assertions.assertEquals(0, results.get(7))
        Assertions.assertEquals(0, results.get(8))
        Assertions.assertEquals(0, results.get(9))
    }

    companion object {
        @OptIn(DelicateCoroutinesApi::class)
        fun testWithContext(nThreads: Int, block: suspend (ctx: CoroutineContext) -> Unit) {
            runBlocking {
                newFixedThreadPoolContext(nThreads, "ctx").use {
                    block(it)
                }
            }
        }
    }
}
