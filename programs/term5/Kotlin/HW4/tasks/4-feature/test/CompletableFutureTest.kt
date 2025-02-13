import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.locks.ReentrantLock
import kotlin.concurrent.withLock
import kotlin.time.Duration.Companion.milliseconds
import kotlinx.coroutines.*
import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.MethodOrderer
import org.junit.jupiter.api.Order
import org.junit.jupiter.api.RepeatedTest
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.TestMethodOrder

@TestMethodOrder(MethodOrderer.OrderAnnotation::class)
class CompletableFutureTest {

    @Test
    @Order(1)
    fun testAwaitDoneFeature() {
        val x = object : CompletableFuture<Int> {
            override val isDone = true
            override val isCancelled = true
            override val value = 42
            override fun cancel() {
                Assertions.fail<Unit>("cancel invocation not expected, because Feature already done")
            }

            override fun handle(handler: ContinuationHandler<Int>) {
                Assertions.fail<Unit>("handle invocation not expected, because Feature already done")
            }
        }

        runBlocking {
            Assertions.assertEquals(42, x.await())
        }

        runBlocking {
            val r1 = x.await()
            val r2 = x.await()

            Assertions.assertEquals(42, r2)
            Assertions.assertEquals(42, r1)
        }
    }

    @Test
    @Order(11)
    fun testAwaitOne() {
        val f = TestFeature()
        runBlocking {
            launch {
                delay(200.milliseconds)
                f.done(1)
            }

            val r = f.await()
            Assertions.assertEquals(1, r)
        }
    }

    @Test
    @Order(12)
    fun testAwaitTwice() {
        val f = TestFeature()
        runBlocking {
            launch {
                delay(200.milliseconds)
                f.done(1)
            }
            Assertions.assertEquals(1, f.await())
            Assertions.assertEquals(1, f.await())
        }
    }

    @RepeatedTest(5)
    @Order(15)
    fun testAwaitParallel() {
        val f = TestFeature()
        runBlocking {
            launch {
                delay(200.milliseconds)
                f.done(1)
            }

            coroutineScope {
                launch {
                    Assertions.assertEquals(1, f.await())
                }
                launch {
                    Assertions.assertEquals(1, f.await())
                }
                launch {
                    Assertions.assertEquals(1, f.await())
                }
            }
        }

        runBlocking {
            Assertions.assertEquals(1, f.await())
        }
    }

    @Test
    @Order(21)
    fun testCancelEvaluation() {
        val f = TestFeature()
        runBlocking {
            val j = launch {
                Assertions.assertEquals(1, f.await())
            }
            delay(100.milliseconds)
            j.cancel()
            Assertions.assertTrue(f.isCancelled)
        }
        Assertions.assertThrows(CancellationException::class.java, {
            runBlocking {
                f.await()
            }
        }, "Invoke value on cancelled Feature should throws CancellationException")
    }

    @Test
    @Order(22)
    fun testCancelEvaluation2() {
        val f = TestFeature()
        runBlocking {
            launch {
                val j = launch {
                    f.await()
                }
                delay(100.milliseconds)
                j.cancel()
            }

            val j1 = async { f.await() }
            Assertions.assertThrows(CancellationException::class.java) { runBlocking { j1.await() } }

            val j2 = launch { f.await() }
            j2.join()
            Assertions.assertTrue(j2.isCancelled)
        }
        runBlocking {
            Assertions.assertThrows(CancellationException::class.java) { runBlocking { f.await() } }
        }
    }

    private class TestFeature : CompletableFuture<Int> {
        @Volatile
        private var computedValue: Int? = null
        private val handlers = ConcurrentLinkedQueue<ContinuationHandler<Int>>()
        private val mutex = ReentrantLock()

        @Volatile
        override var isCancelled: Boolean = false
            private set

        fun done(value: Int) {
            mutex.withLock {
                computedValue = value
                while (handlers.isNotEmpty()) {
                    handlers.poll().invoke(value)
                }
            }
        }

        override val isDone: Boolean
            get() = computedValue != null

        override val value: Int
            get() = computedValue!!

        override fun cancel() {
            mutex.withLock {
                isCancelled = true
                for (handler in handlers) {
                    handler.cancel()
                }
            }
        }

        override fun handle(handler: ContinuationHandler<Int>) {
            mutex.withLock {
                if (isCancelled) {
                    throw CancellationException()
                }
                computedValue?.let { handler.invoke(it) } ?: handlers.add(handler)
            }
        }
    }
}
