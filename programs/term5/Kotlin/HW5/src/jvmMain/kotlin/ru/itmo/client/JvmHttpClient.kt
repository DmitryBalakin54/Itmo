package ru.itmo.client

import java.net.http.HttpClient as JvmHttpClient
import java.net.http.HttpRequest as JvmHttpRequest
import java.net.http.HttpResponse as JvmHttpResponse
import java.net.URI

class JvmHttpClient : HttpClient {
    private val client = JvmHttpClient.newHttpClient()

    override suspend fun request(method: HttpMethod, request: HttpRequest): HttpResponse {
        val javaRequestBuilder = JvmHttpRequest.newBuilder()
            .uri(URI.create(request.url))
            .apply {
                request.headers.value.forEach { (key, value) -> this.setHeader(key, value) }
                when (method) {
                    HttpMethod.GET -> this.GET()
                    HttpMethod.POST -> this.POST(JvmHttpRequest.BodyPublishers.ofString(request.body?.let { it.decodeToString() }))
                    HttpMethod.PUT -> this.PUT(JvmHttpRequest.BodyPublishers.ofString(request.body?.let { it.decodeToString() }))
                    HttpMethod.DELETE -> this.DELETE()
                }
            }

        val javaResponse = client.send(javaRequestBuilder.build(), JvmHttpResponse.BodyHandlers.ofString())


        val headers = javaResponse.headers().map().mapValues { (_, value) ->
            value.joinToString(",")
        }

        return HttpResponse(
            HttpStatus(javaResponse.statusCode()),
            HttpHeaders(headers),
            javaResponse.body().toByteArray()
        )
    }

    override fun close() {}
}