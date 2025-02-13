@file:OptIn(ExperimentalForeignApi::class, ExperimentalCoroutinesApi::class)

package ru.itmo.client

import kotlinx.cinterop.*
import kotlinx.coroutines.ExperimentalCoroutinesApi
import libcurl.*
import platform.posix.size_t

// Hint: See examples by the link - https://curl.se/libcurl/c/example.html

private val curlGlobalInit: Int = curl_global_init(CURL_GLOBAL_ALL.convert()).convert()

typealias HeaderData = Unit // TODO: replace

private fun onCurlHeadersReceived(
    buffer: CPointer<ByteVar>,
    size: size_t,
    count: size_t,
    userdata: COpaquePointer
): Int {
    val headerData = userdata.fromCPointer<HeaderData>()
    TODO("transfer data from buffer to headerData")
}

typealias RequestData = Unit // TODO: replace

private fun onRequestTransfer(
    buffer: CPointer<ByteVar>,
    size: size_t,
    count: size_t,
    userdata: COpaquePointer
): Int {
    val requestData = userdata.fromCPointer<RequestData>()
    TODO("transfer data from requestData to buffer")
}

typealias ResponseData = Unit // TODO: replace

fun onResponseTransfer(
    buffer: CPointer<ByteVar>,
    size: size_t,
    count: size_t,
    userdata: COpaquePointer
): Int {
    val responseData = userdata.fromCPointer<ResponseData>()
    TODO("transfer data from buffer to responseData")
}

private inline fun <T : Any> T.asStablePointer(): COpaquePointer = StableRef.create(this).asCPointer()

private inline fun <reified T : Any> COpaquePointer.fromCPointer(): T = asStableRef<T>().get()

class CurlHttpClient : HttpClient {
    override suspend fun request(method: HttpMethod, request: HttpRequest): HttpResponse {
        if (curlGlobalInit != 0) {
            throw CurlException("curl_global_init() returned non-zero verify: $curlGlobalInit")
        }
        TODO("Implement requests sending")
    }

    override fun close() {
        TODO("close active resources")
    }
}

typealias CurlEasyPointer = COpaquePointer

data class Curl(val curl: CurlEasyPointer) {
    private val responseHeaders: HeaderData = Unit
    private val responseBody: ResponseData = Unit

    fun setupRequest(httpMethod: HttpMethod, request: HttpRequest) {
        setupMethod(httpMethod, request.bodySize)
        setupUrl(request.url)
        setupHeaders(request.headers)
        setupRequestTransfer(request.body, request.bodySize)
        setupResponseTransfer()
    }

    fun perform(): HttpResponse {
        curl_easy_perform(curl).verify()
        TODO("Convert libcurl data to HttpResponse")
    }

    private fun setupMethod(httpMethod: HttpMethod, size: Long) {
        TODO()
    }

    private fun setupUrl(url: String) {
        TODO()
    }

    private fun setupHeaders(headers: HttpHeaders) {
        TODO()
    }

    private fun setupRequestTransfer(body: ByteArray?, size: Long) {
        TODO()
    }

    private fun setupResponseTransfer() {
        TODO()
    }
}

private val HttpRequest.bodySize: Long
    get() = body?.size?.toLong() ?: -1L

private fun CURLcode.verify() {
    if (this != CURLE_OK) {
        throw CurlException("Unexpected curl verify: ${curl_easy_strerror(this)?.toKString()}")
    }
}


class CurlException(message: String) : RuntimeException(message)