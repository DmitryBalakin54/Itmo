import kotlinx.coroutines.*
import kotlinx.coroutines.channels.*

typealias ImageGenerator = (query: String) -> ByteArray

class ImageProcessor(
    private val parallelism: Int,
    private val requests: ReceiveChannel<String>,
    private val publications: SendChannel<Pair<String, ByteArray>>,
    private val generator: ImageGenerator,
) {
    fun run(scope: CoroutineScope) {
        scope.launch {
            val jobs = mutableListOf<Job>()
            val queriesChanel = Channel<String>()

            scope.launch {
                val imageQueries = mutableSetOf<String>()
                for (query in requests) {
                    if (imageQueries.add(query)) {
                        queriesChanel.send(query)
                    }
                }
                queriesChanel.close()
            }

            repeat(parallelism) {
                val job = scope.launch {
                    for (query in queriesChanel) {
                        publications.send(query to generator(query))
                    }
                }

                jobs.add(job)
            }

            jobs.joinAll()
        }
    }
}
