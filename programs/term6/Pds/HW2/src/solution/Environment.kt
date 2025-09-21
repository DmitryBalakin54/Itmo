package solution

interface Environment {
    val processId: Int
    val neighbours: Map<Int, Long>
    fun finishExecution()
    fun send(dstId: Int, message: Any)
}