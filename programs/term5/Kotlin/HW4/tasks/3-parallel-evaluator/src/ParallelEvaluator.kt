import kotlin.coroutines.CoroutineContext
import kotlinx.coroutines.*

class ParallelEvaluator {
    suspend fun run(task: Task, n: Int, context: CoroutineContext) {
        coroutineScope {
            repeat(n) {
                launch(context) {
                    try {
                        task.run(it)
                    } catch (e: Exception) {
                        throw TaskEvaluationException(e)
                    }
                }
            }
        }
    }
}
