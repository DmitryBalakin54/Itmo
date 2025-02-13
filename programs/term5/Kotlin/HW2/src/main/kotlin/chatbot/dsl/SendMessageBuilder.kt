package chatbot.dsl

import chatbot.api.Keyboard
import chatbot.api.MessageId

@BotDslMarker
class SendMessageBuilder {
    var keyboard: Keyboard? = null
    var text: String = ""
    var replyTo: MessageId? = null

    fun removeKeyboard() {
        keyboard = Keyboard.Remove
    }

    fun withKeyboard(init: KeyboardBuilder.() -> Unit) {
        val builder = KeyboardBuilder().apply(init)
        keyboard = Keyboard.Markup(builder.oneTime, builder.keyboard)
    }
}
