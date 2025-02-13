package chatbot.dsl

import chatbot.api.ChatContext
import chatbot.api.Client
import chatbot.api.Message
import chatbot.bot.MessageHandler
import chatbot.bot.MessageProcessor

@OtherMarker
open class HandlersBuilder<C : ChatContext>(
    val contextPredicate: (ChatContext?) -> Boolean,
    open val client: Client,
) {
    open var elements: MutableList<MessageHandler<out ChatContext>> = mutableListOf()

    private fun add(
        predicate: (message: Message) -> Boolean,
        process: MessageProcessor<C>,
    ) {
        val newPredicate: (message: Message, context: ChatContext?) -> Boolean =
            { message, context -> contextPredicate(context) && predicate(message) }

        elements.add(MessageHandler(newPredicate, process))
    }

    open fun onCommand(
        command: String,
        process:
            @OtherMarker
            @BotDslMarker
            MessageProcessor<C>,
    ) {
        val predicate = { message: Message -> message.text.startsWith("/$command") }
        add(predicate, process)
    }

    fun onMessage(messageTextExactly: String, process: @OtherMarker MessageProcessor<C>) {
        val predicate = { message: Message -> message.text == messageTextExactly }
        add(predicate, process)
    }

    fun onMessage(process: @OtherMarker MessageProcessor<C>) {
        val predicate = { _: Message -> true }
        add(predicate, process)
    }

    fun onMessagePrefix(prefix: String, process: @OtherMarker MessageProcessor<C>) {
        val predicate = { message: Message -> message.text.startsWith(prefix) }
        add(predicate, process)
    }

    fun onMessageContains(text: String, process: @OtherMarker MessageProcessor<C>) {
        val predicate = { message: Message -> message.text.contains(text) }
        add(predicate, process)
    }

    fun onMessage(predicate: (Message) -> Boolean, process: @OtherMarker MessageProcessor<C>) {
        val newPredicate = { message: Message -> predicate(message) }
        add(newPredicate, process)
    }
}
