package solution

interface DijkstraProcess {
    fun onComputationStart()
    fun onMessage(srcId: Int, message: Any)
    val distance: Long?
}
