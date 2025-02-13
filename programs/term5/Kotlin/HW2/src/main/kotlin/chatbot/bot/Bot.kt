package chatbot.bot

import chatbot.api.*

class Bot(
    override val logLevel: LogLevel,
    private val messageHandlers: List<MessageHandler<ChatContext?>>,
    private val contextManager: ChatContextsManager?,
    private val client: Client,
) : ChatBot {

    override fun processMessages(message: Message) {
        if (logLevel == LogLevel.INFO) {
            println("[INFO] precessing message $message")
        }
        val context = contextManager?.getContext(message.chatId)
        for (handler in messageHandlers) {
            if (handler.predicate(message, context)) {
                val setContest: (ChatContext?) -> Unit = { contextManager?.setContext(message.chatId, it) }
                MessageProcessorContext(message, client, context, setContest)
                    .apply(handler.processor)
                break
            }
        }
    }
}
