package ru.itmo.client

import kotlinx.browser.window
import kotlinx.coroutines.await
import org.w3c.fetch.Headers
import org.w3c.fetch.RequestInit
import kotlin.js.Promise

private enum class Platform { Node, Browser }

private val platform: Platform
    get() {
        val hasNodeApi = js(
            """
            (typeof process !== 'undefined' 
                && process.versions != null 
                && process.versions.node != null) ||
            (typeof window !== 'undefined' 
                && typeof window.process !== 'undefined' 
                && window.process.versions != null 
                && window.process.versions.node != null)
            """
        ) as Boolean
        return if (hasNodeApi) Platform.Node else Platform.Browser
    }

private val nodeFetch: dynamic
    get() = js("eval('require')('node-fetch')")

private fun RequestInit.asNodeOptions(): dynamic =
    js("Object").assign(js("Object").create(null), this)


class JsHttpClient : HttpClient {
    override suspend fun request(method: HttpMethod, request: HttpRequest): HttpResponse {
        val response = when (platform) {
            Platform.Browser -> window.fetch(request.url, toRequestInit(method, request))
            Platform.Node -> fetch(request.url, toRequestInit(method, request).asNodeOptions())
        }.await()

        val body = (response.text() as Promise<String>).await()
        val headers = mutableMapOf<String, String>()
        (js("Array.from(response.headers.entries())") as Array<Array<String>>)
            .forEach { entry -> headers[entry[0]] = entry[1] }

        return HttpResponse(
            HttpStatus(response.status as Int),
            HttpHeaders(headers),
            body.encodeToByteArray()
        )
    }

    override fun close() {}
}


private fun toRequestInit(method: HttpMethod, request: HttpRequest): RequestInit {
    val requestInit = RequestInit(
        method.name,
        request.headers.value.toJsHeaders(),
        request.body?.let { it.decodeToString() }
    )

    return requestInit
}

private fun Map<String, String>.toJsHeaders(): Headers {
    val headers = Headers()
    forEach { (key, value) -> headers.append(key, value) }
    return headers
}

@JsModule("node-fetch")
@JsNonModule
external fun fetch(input: String, init: dynamic): Promise<dynamic>

