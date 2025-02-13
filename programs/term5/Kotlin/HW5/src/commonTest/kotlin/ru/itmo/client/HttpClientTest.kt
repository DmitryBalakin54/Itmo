package ru.itmo.client

import kotlinx.coroutines.test.runTest
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.time.Duration.Companion.seconds

class HttpClientTest {
    @Test
    fun testGET() = runTest(timeout = 15.seconds) {
        HttpClient().use { client ->
            val response = client.request(HttpMethod.GET, HttpRequest("$JSON_PLACEHOLDER_HOST/posts/1"))
            response.validateThat(
                body = """
                |{
                |  "userId": 1,
                |  "id": 1,
                |  "title": "sunt aut facere repellat provident occaecati excepturi optio reprehenderit",
                |  "body": "quia et suscipit\nsuscipit recusandae consequuntur expedita et cum\nreprehenderit molestiae ut ut quas totam\nnostrum rerum est autem sunt rem eveniet architecto"
                |}
                """.trimMargin()
            )
        }
    }

    @Test
    fun testPUT() = runTest(timeout = 15.seconds) {
        HttpClient().use { client ->
            val response = client.request(
                HttpMethod.PUT,
                HttpRequest(
                    url = "$JSON_PLACEHOLDER_HOST/posts/1",
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
            response.validateThat(
                body = """
                |{
                |  "id": 1,
                |  "title": "foo",
                |  "body": "bar",
                |  "userId": 1
                |}
                """.trimMargin()
            )
        }
    }

    @Test
    fun testPOST() = runTest(timeout = 15.seconds) {
        HttpClient().use { client ->
            val response = client.request(
                HttpMethod.POST,
                HttpRequest(
                    url = "$JSON_PLACEHOLDER_HOST/posts",
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
            response.validateThat(
                status = HttpStatus(value = 201),
                body = """
                    |{
                    |  "title": "foo",
                    |  "body": "bar",
                    |  "userId": 1,
                    |  "id": 101
                    |}
                """.trimMargin()
            )
        }
    }

    @Test
    fun testDELETE() = runTest(timeout = 15.seconds) {
        HttpClient().use { client ->
            val response = client.request(
                HttpMethod.DELETE,
                HttpRequest(url = "$JSON_PLACEHOLDER_HOST/posts/1"),
            )
            response.validateThat(
                body = """
                    |{}
                """.trimMargin()
            )
        }
    }

    private fun HttpResponse.validateThat(status: HttpStatus = HttpStatus(value = 200), body: String?) {
        assertEquals(actual = this.status, expected = status)
        assertEquals(actual = this.body?.decodeToString(), expected = body)
    }

    companion object {
        private const val JSON_PLACEHOLDER_HOST = "https://jsonplaceholder.typicode.com"
    }
}
