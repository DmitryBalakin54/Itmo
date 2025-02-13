package chatbot.dsl

import chatbot.api.ChatContext
import chatbot.api.ChatId
import chatbot.api.Keyboard
import chatbot.bot.MessageProcessorContext

fun <C : ChatContext?> MessageProcessorContext<C>.sendMessage(chatId: ChatId, text: String) {
    client.sendMessage(chatId, text)
}

fun <C : ChatContext?> MessageProcessorContext<C>.sendMessage(chatId: ChatId, init: SendMessageBuilder.() -> Unit) {
    val builder = SendMessageBuilder().apply(init)

    if (isNonEmptyMessage(builder) || builder.keyboard == Keyboard.Remove) {
        client.sendMessage(chatId, builder.text, builder.keyboard, builder.replyTo)
    }
}

private fun isNonEmptyMessage(builder: SendMessageBuilder): Boolean {
    val textNonEmpty = builder.text.isNotBlank()
    val keyboardIsMarkUp = builder.keyboard is Keyboard.Markup
    val keyboardNonEmpty = (builder.keyboard as? Keyboard.Markup)?.keyboard?.flatten()?.isNotEmpty() == true

    return textNonEmpty || keyboardIsMarkUp && keyboardNonEmpty
}
