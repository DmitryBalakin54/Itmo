package ru.itmo

import ru.itmo.client.*

private fun HttpResponse.printDebug() {
    println(status)
    println(headers)
    println(body?.decodeToString())
}

@OptIn(ExperimentalStdlibApi::class)
suspend fun scenario() {
    HttpClient().use { client ->
        val response = client.request(HttpMethod.GET, HttpRequest("https://jsonplaceholder.typicode.com/posts/1"))
        response.printDebug()
    }

    HttpClient().use { client ->
        val response = client.request(
            HttpMethod.PUT,
            HttpRequest(
                url = "https://jsonplaceholder.typicode.com/posts/1",
                headers = HttpHeaders(
                    value = mapOf("Content-Type" to "application/json"),
                ),
                body = """
                    |{
                    |    "id": 1,
                    |    "title": "foo",
                    |    "body": "bar",
                    |    "userId": 1
                    |}
                """.trimMargin().encodeToByteArray()
            ),
        )
        response.printDebug()
    }

    HttpClient().use { client ->
        val response = client.request(
            HttpMethod.POST,
            HttpRequest(
                url = "https://jsonplaceholder.typicode.com/posts",
                headers = HttpHeaders(
                    value = mapOf("Content-Type" to "application/json"),
                ),
                body = """
                    |{
                    |    "title": "foo",
                    |    "body": "bar",
                    |    "userId": 1
                    |}
                """.trimMargin().encodeToByteArray()
            ),
        )
        response.printDebug()
    }

    HttpClient().use { client ->
        val response = client.request(
            HttpMethod.DELETE,
            HttpRequest(url = "https://jsonplaceholder.typicode.com/posts/1"),
        )
        response.printDebug()
    }
}
