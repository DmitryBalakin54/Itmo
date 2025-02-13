import kotlin.time.Duration.Companion.seconds
import kotlinx.coroutines.*

fun CoroutineScope.runApplication(
    runUI: suspend () -> Unit,
    runApi: suspend () -> Unit,
) {
    launch { runUI() }
    launch {
        while (true) {
            try {
                runApi()
                break
            } catch (e: CancellationException) {
                break
            } catch (e: Exception) {
                delay(1.seconds)
            }
        }
    }
}
