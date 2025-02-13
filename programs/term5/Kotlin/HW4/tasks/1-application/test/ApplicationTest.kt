import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicInteger
import kotlin.time.Duration.Companion.milliseconds
import kotlin.time.Duration.Companion.seconds
import kotlinx.coroutines.*
import org.junit.jupiter.api.*

@TestMethodOrder(MethodOrderer.OrderAnnotation::class)
class ApplicationTest {

    @Test
    @Order(1)
    fun testBasic1() {
        val uiResult = AtomicBoolean(false)
        val apiResult = AtomicBoolean(false)

        runBlocking {
            runApplication({
                uiResult.set(true)
            }, {
                delay(200.milliseconds)
                apiResult.set(true)
            })
        }
        Assertions.assertTrue(uiResult.get(), "ui should complete successful")
        Assertions.assertTrue(apiResult.get(), "api should complete successful")
    }

    @Test
    @Order(2)
    fun testBasic2() {
        val uiResult = AtomicBoolean(false)
        val apiResult = AtomicBoolean(false)

        runBlocking {
            runApplication({
                delay(200.milliseconds)
                uiResult.set(true)
            }, {
                apiResult.set(true)
            })
        }
        Assertions.assertTrue(uiResult.get(), "ui should complete successful")
        Assertions.assertTrue(apiResult.get(), "api should complete successful")
    }

    @Test
    @Order(4)
    fun testFailUi() {
        val apiResult = AtomicBoolean(false)

        var handled: Throwable? = null
        val handler = CoroutineExceptionHandler { _, throwable ->
            handled = throwable
        }
        runBlocking {
            val scope = CoroutineScope(Dispatchers.Default + handler)
            scope.launch {
                runApplication({
                    delay(100.milliseconds)
                    throw RuntimeException("Unknown ui error")
                }, {
                    delay(2.seconds)
                    apiResult.set(true)
                })
            }.join()
        }
        Assertions.assertFalse(apiResult.get(), "api should not complete successful because ui throws exception")
        Assertions.assertEquals("Unknown ui error", handled?.message, "ui should throws exception")
    }

    @Test
    @Order(4)
    fun testFailApi() {
        val restartCount = 4
        val uiResult = AtomicBoolean(false)
        val apiResult = AtomicBoolean(false)
        val apiCounter = AtomicInteger(0)

        runBlocking {
            runApplication({
                delay(50.milliseconds * (restartCount + 1))
                uiResult.set(true)
            }, {
                if (apiCounter.incrementAndGet() < restartCount) {
                    delay(50.milliseconds)
                    throw RuntimeException("Api error")
                }
                apiResult.set(true)
            })
        }
        Assertions.assertEquals(
            restartCount,
            apiCounter.get(),
            "api should throws $restartCount exceptions and then complete successful",
        )
        Assertions.assertTrue(uiResult.get(), "ui should complete successful")
        Assertions.assertTrue(apiResult.get(), "api should complete successful (after retries)")
    }
}

// TODO: add more tests
