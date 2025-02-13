import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicInteger
import org.junit.jupiter.api.*

@TestMethodOrder(MethodOrderer.OrderAnnotation::class)
class OnceTest {
    @Test
    @Order(1)
    fun oneRun() {
        val once = Once()
        var ok = false
        once.run {
            ok = true
        }
        Assertions.assertTrue(ok, "Once.run should be invoked")
    }

    @Test
    @Order(2)
    fun runTwiceSequential() {
        val once = Once()
        var phase = 0
        once.run {
            phase = 1
        }
        once.run {
            phase = 2
        }
        Assertions.assertEquals(1, phase, "should be invoked first run in Once")
    }

    @Test
    @Order(3)
    fun runMultipleParallel() {
        for (repeat in 1..1000) {
            val once = Once()
            val counter = AtomicInteger(0)
            testRunParallel(5, 100) {
                once.run { counter.incrementAndGet() }
            }
            Assertions.assertEquals(1, counter.get(), "body should be invoked once")
        }
    }

    @Test
    @Order(4)
    fun runMultipleParallelWithFail() {
        for (repeat in 1..1000) {
            val once = Once()
            val counter = AtomicInteger(0)
            val wasThrown = AtomicBoolean(false)

            testRunParallel(2, 100) { i ->
                try {
                    once.run {
                        counter.incrementAndGet()
                        if (i / 2 < 2) {
                            throw IllegalArgumentException()
                        }
                    }
                } catch (e: IllegalArgumentException) {
                    wasThrown.set(true)
                }
            }
            Assertions.assertEquals(1, counter.get(), "body should be invoked once")
            Assertions.assertTrue(wasThrown.get(), "exception in one run should be thrown")
        }
    }
}
