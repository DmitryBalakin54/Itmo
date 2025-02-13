package ru.itmo.client

class HttpRequest(
    val url: String,
    val headers: HttpHeaders = HttpHeaders(),
    val body: ByteArray? = null,
)
