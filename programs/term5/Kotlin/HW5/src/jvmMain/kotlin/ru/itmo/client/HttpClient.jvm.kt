package ru.itmo.client

actual fun HttpClient(): HttpClient = JvmHttpClient()
