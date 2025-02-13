import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicIntegerArray
import kotlin.time.Duration.Companion.milliseconds
import kotlinx.coroutines.*
import kotlinx.coroutines.channels.Channel
import kotlinx.coroutines.channels.toList
import kotlinx.coroutines.flow.*
import org.junit.jupiter.api.*

@TestMethodOrder(MethodOrderer.OrderAnnotation::class)
class ImageProcessorTest {

    @Test
    @Order(1)
    fun simpleGeneration() {
        val requests = Channel<String>(2)
        val publications = Channel<Pair<String, ByteArray>>(2)
        val processor = ImageProcessor(1, requests, publications, IMAGE_GENERATOR)
        runBlocking {
            requests.send("cat")
            requests.send("cute cat")
            requests.close()

            processor.run(this)

            assertPublication("cat", publications.receive())
            assertPublication("cute cat", publications.receive())
            publications.close()
        }
    }

    @Test
    @Order(2)
    fun simpleParallelSequentialGeneration() {
        val requests = Channel<String>(1)
        val publications = Channel<Pair<String, ByteArray>>(4)
        val processor = ImageProcessor(2, requests, publications, IMAGE_GENERATOR)
        testRunParallel(10) {
            processor.run(this)

            launch {
                requests.send("cat")
                delay(50.milliseconds)
                requests.send("dog")
                delay(50.milliseconds)
                requests.send("otter")
                delay(50.milliseconds)
                requests.send("Ravil the hedgehog")
                requests.close()
            }

            assertPublication("cat", publications.receive())
            assertPublication("dog", publications.receive())
            assertPublication("otter", publications.receive())
            assertPublication("Ravil the hedgehog", publications.receive())
            publications.close()
        }
    }

    @Test
    @Order(3)
    fun simpleParallelGeneration() {
        val requests = Channel<String>(3)
        val publications = Channel<Pair<String, ByteArray>>(12)
        val processor = ImageProcessor(2, requests, publications, IMAGE_GENERATOR)
        testRunParallel(10) {
            launch {
                processor.run(this)
            }

            launch {
                for (i in 1..12) {
                    requests.send("Shiba inu $i")
                }
                requests.close()
            }

            launch {
                val collected = publications.consumeAsFlow().take(12).map { it.first }.toSet()
                Assertions.assertEquals((1..12).map { "Shiba inu $it" }.toSet(), collected)
                publications.close()
            }
        }
    }

    @Test
    @Order(7)
    fun testMore() {
        val requests = Channel<String>(Channel.UNLIMITED)
        val publications = Channel<Pair<String, ByteArray>>(Channel.UNLIMITED)
        val processor = ImageProcessor(10, requests, publications) {
            it.toByteArray()
        }

        testRunParallel(10) {
            launch {
                processor.run(this)
            }

            (0..4).map { i ->
                launch {
                    for (j in 0..<20) {
                        requests.send("${(i * 20 + j) % 25 + 1} cats")
                    }
                }
            }.joinAll()
            requests.close()

            delay(200.milliseconds)
            publications.close()
            val expected = (1..25).map { "$it cats" }.toSet()
            Assertions.assertEquals(expected, publications.toList().map { it.second.decodeToString() }.toSet())
        }
    }

    @Test
    @Order(11)
    fun parallelLimit() {
        val requests = Channel<String>(Channel.UNLIMITED)
        val publications = Channel<Pair<String, ByteArray>>(Channel.UNLIMITED)

        val parallelism = 3
        val counter = AtomicInteger(0)
        val maxParallel = AtomicInteger(0)
        val processor = ImageProcessor(parallelism, requests, publications) {
            val actual = counter.incrementAndGet()
            if (actual > parallelism) {
                maxParallel.set(actual)
            }
            Thread.sleep(10)
            counter.decrementAndGet()
            ByteArray(0)
        }

        testRunParallel(10) {
            launch {
                processor.run(this)
            }

            for (thread in 1..5) {
                launch {
                    for (i in 1..20) {
                        requests.send("$thread-$i")
                    }
                }
            }

            launch {
                var collected = 0
                while (collected < 20 * 5) {
                    publications.receive()
                    collected++
                }
                requests.close()
                publications.close()
            }
        }

        Assertions.assertTrue(
            maxParallel.get() == 0,
            "ImageProcessor process more than $parallelism parallel requests (was ${maxParallel.get()})",
        )
    }

    @Test
    @Order(21)
    fun cacheSimple() {
        val requests = Channel<String>(Channel.UNLIMITED)
        val publications = Channel<Pair<String, ByteArray>>(Channel.UNLIMITED)
        val counter = AtomicInteger(0)

        val processor = ImageProcessor(1, requests, publications) {
            counter.incrementAndGet()
            it.toByteArray()
        }

        testRunParallel(10) {
            launch {
                processor.run(this)
            }

            launch {
                requests.send("corgi drinks cappuccino")
                requests.send("corgi drinks cappuccino")
                requests.send("corgi drinks cappuccino")
                requests.close()
            }

            delay(100.milliseconds)
            publications.close()
            Assertions.assertEquals(
                listOf("corgi drinks cappuccino"),
                publications.toList().map { it.first },
                "only one publication expected",
            )
            Assertions.assertEquals(1, counter.get(), "Only one generation expected")
        }
    }

    @Test
    @Order(22)
    fun cacheMultiple() {
        val requests = Channel<String>(Channel.UNLIMITED)
        val publications = Channel<Pair<String, ByteArray>>(Channel.UNLIMITED)
        val counter = AtomicIntegerArray(10)

        val processor = ImageProcessor(10, requests, publications) {
            counter.incrementAndGet(it.substring(5).toInt())
            it.toByteArray()
        }

        testRunParallel(20) {
            launch {
                processor.run(this)
            }

            (0..<20).map { i ->
                launch {
                    for (j in 0..<10) {
                        requests.send("corgi${(i + j) % 10}")
                    }
                    println("done $i")
                }
            }.joinAll()
            delay(100.milliseconds)
            println("done")
            requests.close()
        }

        for (i in 0..<10) {
            Assertions.assertEquals(1, counter.get(i), "image by request corgi$i should be generated once")
        }
    }

    companion object {
        private val IMAGE_GENERATOR: ImageGenerator = {
            Thread.sleep(50)
            "Generated $it".toByteArray()
        }

        private fun assertPublication(expected: String, actual: Pair<String, ByteArray>, message: String? = null) {
            Assertions.assertEquals(expected, actual.first, message)
            Assertions.assertEquals("Generated $expected", actual.second.decodeToString(), message)
        }
    }
}
