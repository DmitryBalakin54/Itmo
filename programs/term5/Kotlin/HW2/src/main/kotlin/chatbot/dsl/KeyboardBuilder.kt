package chatbot.dsl

import chatbot.api.Keyboard.Button

@BotDslMarker
class KeyboardBuilder {
    var oneTime: Boolean = false
    var keyboard: MutableList<MutableList<Button>> = mutableListOf()

    fun row(init: RowBuilder.() -> Unit) {
        keyboard.add(mutableListOf())
        RowBuilder(keyboard.last()).apply(init)
    }
}
