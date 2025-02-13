package ru.itmo.client

class HttpResponse(
    val status: HttpStatus,
    val headers: HttpHeaders,
    val body: ByteArray?,
)
