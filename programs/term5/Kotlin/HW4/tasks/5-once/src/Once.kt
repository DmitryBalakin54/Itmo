import kotlinx.coroutines.sync.Mutex

class Once {
    private var was = false
    private val mutex = Mutex()

    fun run(block: () -> Unit) {
        if (!was) {
            if (mutex.tryLock()) {
                was = true
                block()
            }
        }
    }
}
