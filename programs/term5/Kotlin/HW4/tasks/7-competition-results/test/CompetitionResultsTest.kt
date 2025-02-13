import kotlin.time.Duration
import kotlin.time.Duration.Companion.minutes
import kotlinx.coroutines.flow.*
import kotlinx.coroutines.runBlocking
import org.junit.jupiter.api.*

@TestMethodOrder(MethodOrderer.OrderAnnotation::class)
class CompetitionResultsTest {
    @Test
    @Order(1)
    fun testOneResult() {
        val f = flow {
            emit(Cutoff(1, 1.minutes))
        }
        runBlocking {
            val results = f.resultsFlow().filterNot { it.results.isEmpty() }.first()
            Assertions.assertEquals(mapOf(1 to 1.minutes), results.results)
        }
    }

    @Test
    @Order(2)
    fun testFive() {
        val f = flow {
            emit(Cutoff(1, 1.minutes))
            emit(Cutoff(2, 2.minutes))
            emit(Cutoff(3, 3.minutes))
            emit(Cutoff(4, 4.minutes))
            emit(Cutoff(5, 5.minutes))
        }

        runBlocking {
            val results = f.resultsFlow().drop(4)
            Assertions.assertEquals(
                mapOf(
                    1 to 1.minutes,
                    2 to 2.minutes,
                    3 to 3.minutes,
                    4 to 4.minutes,
                    5 to 5.minutes,
                ),
                results.first().results,
            )
        }
    }

    @Test
    @Order(3)
    fun testOnMutableFlow() {
        val f = MutableSharedFlow<Cutoff>(replay = 10, extraBufferCapacity = 10)
        val results = f.resultsFlow()
        runBlocking {
            val expected = mutableListOf<Map<Int, Duration>>()
            val current = mutableMapOf<Int, Duration>()

            f.emit(Cutoff(5, 5.minutes))
            current[5] = 5.minutes
            expected += current.toMap()
            assertResults(expected, results.take(1))

            f.emit(Cutoff(1, 1.minutes))
            current[1] = 1.minutes
            expected += current.toMap()
            assertResults(expected, results.take(2))

            f.emit(Cutoff(2, 2.minutes))
            current[2] = 2.minutes
            expected += current.toMap()
            assertResults(expected, results.take(3))

            f.emit(Cutoff(3, 3.minutes))
            current[3] = 3.minutes
            expected += current.toMap()
            assertResults(expected, results.take(4))

            f.emit(Cutoff(4, 4.minutes))
            current[4] = 4.minutes
            expected += current.toMap()
            assertResults(expected, results.take(5))
        }
    }

    @Test
    @Order(11)
    fun testScoreboardSimple() {
        val expected = listOf(
            ScoreboardRow(1, 1, 2.minutes),
            ScoreboardRow(2, 3, 4.minutes),
            ScoreboardRow(3, 2, 6.minutes),
        )

        runBlocking {
            flow {
                emit(Cutoff(1, 2.minutes))
                emit(Cutoff(3, 4.minutes))
                emit(Cutoff(2, 6.minutes))
            }.resultsFlow().scoreboard().collectIndexed { index, value ->
                Assertions.assertEquals(index + 1, value.rows.size, "scoreboard should have correct number of rows")
                Assertions.assertEquals(
                    expected.subList(0, index + 1),
                    value.rows,
                    "scoreboard should have correct rows",
                )
            }
        }
    }

    companion object {
        suspend fun assertResults(expected: List<Map<Int, Duration>>, results: Flow<Results>) {
            val actual = results.toList().map { it.results }
            Assertions.assertEquals(expected, actual)
        }
    }
}
