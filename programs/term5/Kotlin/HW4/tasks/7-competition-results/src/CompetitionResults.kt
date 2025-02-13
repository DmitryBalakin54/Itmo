import kotlin.time.Duration
import kotlinx.coroutines.flow.*

fun Flow<Cutoff>.resultsFlow(): Flow<Results> =
    scan(mapOf<Int, Duration>()) { currentResults, cutoff ->
        currentResults + (cutoff.number to cutoff.time)
    }.filter { it.isNotEmpty() }.map { Results(it.toMap()) }

fun Flow<Results>.scoreboard(): Flow<Scoreboard> =
    map { results ->
        Scoreboard(
            results.results.entries
                .sortedBy { it.value }
                .mapIndexed { index, (key, value) ->
                    ScoreboardRow(index + 1, key, value)
                },
        )
    }
