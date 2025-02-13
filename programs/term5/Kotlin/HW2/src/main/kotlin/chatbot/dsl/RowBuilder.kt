package chatbot.dsl

import chatbot.api.Keyboard.Button

@BotDslMarker
class RowBuilder(private var keyboardString: MutableList<Button>) {
    fun button(text: String) {
        keyboardString.add(Button(text))
    }

    operator fun String.unaryMinus() {
        keyboardString.add(Button(this))
    }
}
