import kotlinx.coroutines.*

class SequentialProcessor(private val handler: (String) -> String) : TaskProcessor {

    private val context = newSingleThreadContext("single")
    private val scope = CoroutineScope(context)

    override suspend fun process(argument: String): String {
        return scope.async { handler(argument) }.await()
    }

    override fun close() {
        scope.cancel()
        context.close()
    }
}
