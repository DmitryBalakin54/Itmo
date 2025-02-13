package ru.itmo.client

@OptIn(ExperimentalStdlibApi::class)
interface HttpClient : AutoCloseable {
    suspend fun request(method: HttpMethod, request: HttpRequest): HttpResponse
}

expect fun HttpClient(): HttpClient