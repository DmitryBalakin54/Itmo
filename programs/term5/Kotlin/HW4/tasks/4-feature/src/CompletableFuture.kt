import kotlin.coroutines.resume
import kotlin.coroutines.resumeWithException
import kotlinx.coroutines.CancellableContinuation
import kotlinx.coroutines.CancellationException
import kotlinx.coroutines.suspendCancellableCoroutine

/**
 * Interface for the result of asynchronous calculations
 */
interface CompletableFuture<T> {
    /**
     * @return true if calculations already done
     */
    val isDone: Boolean

    /**
     * @return true if calculations is cancelled
     */
    val isCancelled: Boolean

    /**
     * @return value of feature
     * @throws IllegalArgumentException if calculations isn't done
     */
    val value: T

    /**
     * cancel calculation if result of feature not needed
     */
    fun cancel()

    /**
     * adds a handler that will be called after the calculation is completed
     * or call handler immediately if calculation already done
     */
    fun handle(handler: ContinuationHandler<T>)
}

class ContinuationHandler<T>(
    @Volatile @JvmField
    var cont: CancellableContinuation<T>?,
) {
    fun invoke(result: T) {
        val cont = this.cont ?: return
        cont.resume(result)
    }

    fun cancel() {
        val localCont = cont
        cont = null
        localCont?.cancel()
    }
}

suspend fun <T> CompletableFuture<T>.await(): T {
    return suspendCancellableCoroutine { cont ->
        if (isDone) {
            cont.resume(value)
            return@suspendCancellableCoroutine
        }

        if (isCancelled) {
            cont.resumeWithException(CancellationException("Future was cancelled."))
            return@suspendCancellableCoroutine
        }

        val handler = ContinuationHandler(cont)
        handle(handler)
        cont.invokeOnCancellation {
            cancel()
        }
    }
}
