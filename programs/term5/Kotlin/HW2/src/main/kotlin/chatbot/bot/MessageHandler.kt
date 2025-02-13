package chatbot.bot

import chatbot.api.ChatContext
import chatbot.api.Message

class MessageHandler<C : ChatContext?>(
    val predicate: (message: Message, context: C) -> Boolean,
    val processor: MessageProcessor<C>,
)
