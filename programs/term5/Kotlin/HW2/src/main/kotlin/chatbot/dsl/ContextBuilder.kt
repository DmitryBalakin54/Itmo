package chatbot.dsl

import chatbot.api.ChatContext
import chatbot.api.Client

@BotDslMarker
class ContextBuilder(override val client: Client) : HandlersBuilder<ChatContext>({ _ -> true }, client) {
    inline fun <reified C : ChatContext> into(init: HandlersBuilder<C>.() -> Unit) {
        val newContextPredicate = { context: ChatContext? -> context is C }
        val builder = HandlersBuilder<C>(newContextPredicate, client).apply(init)
        elements += builder.elements
    }

    inline infix fun <reified C : ChatContext> C.into(init: HandlersBuilder<C>.() -> Unit) {
        val newContextPredicate = { context: ChatContext? -> context == this }
        val builder = HandlersBuilder<C>(newContextPredicate, client).apply(init)
        elements += builder.elements
    }
}
