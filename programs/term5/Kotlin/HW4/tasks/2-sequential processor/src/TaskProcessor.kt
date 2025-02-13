interface TaskProcessor : AutoCloseable {
    suspend fun process(argument: String): String
}
