import kotlin.time.Duration

data class Cutoff(val number: Int, val time: Duration)

@JvmInline
value class Results(val results: Map<Int, Duration>)

data class ScoreboardRow(val rank: Int, val number: Int, val time: Duration)

@JvmInline
value class Scoreboard(val rows: List<ScoreboardRow>)
