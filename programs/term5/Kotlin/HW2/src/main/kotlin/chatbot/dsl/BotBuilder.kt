package chatbot.dsl

import chatbot.api.*
import chatbot.bot.Bot
import chatbot.bot.MessageHandler

@BotDslMarker
class BotBuilder(var client: Client) {
    fun use(contextManager: ChatContextsManager?) {
        this.contextManager = contextManager
    }

    @Suppress("UNCHECKED_CAST")
    fun behaviour(init: ContextBuilder.() -> Unit) {
        val builder = ContextBuilder(client).apply(init)
        messageHandlers += builder.elements as List<MessageHandler<ChatContext?>>
    }

    fun use(level: LogLevel) {
        logLevel = level
    }

    operator fun LogLevel.unaryPlus() {
        logLevel = this
    }

    var logLevel = LogLevel.ERROR
    var messageHandlers: List<MessageHandler<ChatContext?>> = mutableListOf()
    var contextManager: ChatContextsManager? = null
}

fun chatBot(client: Client, init: BotBuilder.() -> Unit): ChatBot {
    val builder = BotBuilder(client).apply(init)
    return Bot(
        logLevel = builder.logLevel,
        messageHandlers = builder.messageHandlers,
        contextManager = builder.contextManager,
        client = builder.client,
    )
}
