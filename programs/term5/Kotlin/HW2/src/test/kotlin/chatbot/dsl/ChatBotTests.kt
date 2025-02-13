package chatbot.dsl

import chatbot.api.ChatContext
import chatbot.api.Keyboard
import chatbot.api.LogLevel
import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.Test

class ChatBotTests : BaseTests() {
    @Test
    fun simpleChatBot() {
        val bot = chatBot(testClient) {
            use(LogLevel.INFO)

            behaviour {
                onCommand("help") {
                    client.sendMessage(message.chatId, "I need help too")
                }

                onMessage {
                    client.sendMessage(message.chatId, "hello, user", replyMessageId = message.id)
                }
            }
        }

        Assertions.assertEquals(LogLevel.INFO, bot.logLevel)

        bot.processMessages(TEST_MESSAGE_1)
        assertMessage(42, "hello, user", expectedReplyMessageId = TEST_MESSAGE_1.id)

        bot.processMessages(TEST_MESSAGE_HELP)
        assertMessage(11, "I need help too")
    }

    @Test
    fun messageBuilder() {
        val bot = chatBot(testClient) {
            behaviour {
                onMessage("removeKeyboard") {
                    sendMessage(message.chatId) {
                        removeKeyboard()
                    }
                }

                onMessage("manualSetKeyboard") {
                    sendMessage(message.chatId) {
                        withKeyboard {
                            keyboard = mutableListOf(
                                mutableListOf(Keyboard.Button(text = "1:1"), Keyboard.Button(text = "1:2")),
                            )
                            keyboard += mutableListOf(Keyboard.Button(text = "2:1"))
                            keyboard.last() += Keyboard.Button(text = "2:2")
                        }
                    }
                }

                onMessage("buildKeyboard") {
                    sendMessage(message.chatId) {
                        withKeyboard {
                            oneTime = true
                            row {
                                button(text = "1:1")
                                button(text = "1:2")
                            }

                            row {
                                -"2:1"
                                -"2:2"
                            }
                        }
                    }
                }

                onMessage("allEmpty") {
                    sendMessage(message.chatId) { }
                }

                onMessage("emptyKeyboard") {
                    sendMessage(message.chatId) {
                        withKeyboard { }
                    }
                }

                onMessage("emptyKeyboardSet") {
                    sendMessage(message.chatId) {
                        withKeyboard {
                            keyboard = mutableListOf(mutableListOf<Keyboard.Button>())
                        }
                    }
                }

                onMessage("emptyKeyboardBuild") {
                    sendMessage(message.chatId) {
                        withKeyboard {
                            row {}
                            row {}
                        }
                    }
                }

                onMessage {
                    sendMessage(message.chatId) {
                        text = "hello, user"
                        replyTo = message.id
                    }
                }
            }
        }

        Assertions.assertEquals(LogLevel.ERROR, bot.logLevel)

        bot.processMessages(TEST_MESSAGE_1)
        assertMessage(42, "hello, user", expectedReplyMessageId = TEST_MESSAGE_1.id)

        bot.testMessage("removeKeyboard")
        assertMessage(1, "", expectedKeyboard = Keyboard.Remove)

        bot.testMessage("manualSetKeyboard")
        assertMessage(
            1,
            "",
            expectedKeyboard = Keyboard.Markup(
                oneTime = false,
                keyboard = listOf(
                    listOf(Keyboard.Button(text = "1:1"), Keyboard.Button(text = "1:2")),
                    listOf(Keyboard.Button(text = "2:1"), Keyboard.Button(text = "2:2")),
                ),
            ),
        )

        bot.testMessage("buildKeyboard")
        assertMessage(
            1,
            "",
            expectedKeyboard = Keyboard.Markup(
                oneTime = true,
                keyboard = listOf(
                    listOf(Keyboard.Button(text = "1:1"), Keyboard.Button(text = "1:2")),
                    listOf(Keyboard.Button(text = "2:1"), Keyboard.Button(text = "2:2")),
                ),
            ),
        )

        bot.testMessage("allEmpty")
        assertNoMessages()

        bot.testMessage("emptyKeyboard")
        assertNoMessages()

        bot.testMessage("emptyKeyboardSet")
        assertNoMessages()

        bot.testMessage("emptyKeyboardBuild")
        assertNoMessages()
    }

    @Test
    fun setupLogLevelUsingPlus() {
        Assertions.assertEquals(
            LogLevel.INFO,
            chatBot(testClient) { +LogLevel.INFO }.logLevel,
        )
        Assertions.assertEquals(
            LogLevel.ERROR,
            chatBot(testClient) { +LogLevel.ERROR }.logLevel,
        )
        Assertions.assertEquals(
            LogLevel.ERROR,
            chatBot(testClient) { }.logLevel,
        )
    }

    @Test
    fun setupLogLevelUsingUse() {
        Assertions.assertEquals(
            LogLevel.INFO,
            chatBot(testClient) { use(LogLevel.INFO) }.logLevel,
        )
        Assertions.assertEquals(
            LogLevel.ERROR,
            chatBot(testClient) { use(LogLevel.ERROR) }.logLevel,
        )
    }

    @Test
    fun basicBehaviourCommands() {
        val bot = chatBot(testClient) {
            behaviour {
                onMessage({ message -> message.text == "ultra_secret_command" }) {
                    client.sendMessage(message.chatId, "my card pin is 7412")
                }

                onCommand("start") {
                    client.sendMessage(message.chatId, "Lets start!")
                }

                onMessage("secret_key") {
                    client.sendMessage(message.chatId, "You really know it?")
                }

                onMessagePrefix("start using dsl") {
                    client.sendMessage(message.chatId, "Ok, it's easy")
                }

                onMessageContains("plagiarize") {
                    client.sendMessage(message.chatId, "ban")
                }

                onMessage {
                    client.sendMessage(message.chatId, "???")
                }

                onCommand("help") {
                    client.sendMessage(message.chatId, "Message should not being send")
                }
            }
        }

        Assertions.assertEquals(LogLevel.ERROR, bot.logLevel)

        bot.testMessage("ultra_secret_command")
        assertMessage(1, "my card pin is 7412")

        bot.testMessage("/start")
        assertMessage(1, "Lets start!")

        bot.testMessage("secret_key")
        assertMessage(1, "You really know it?")

        bot.testMessage("secret_key ")
        assertMessage(1, "???")

        bot.testMessage(" secret_key")
        assertMessage(1, "???")

        bot.testMessage("start using dsl today")
        assertMessage(1, "Ok, it's easy")

        bot.testMessage("start using dsl")
        assertMessage(1, "Ok, it's easy")

        bot.testMessage("plagiarize")
        assertMessage(1, "ban")

        bot.testMessage("ask group mate to plagiarize solution")
        assertMessage(1, "ban")

        bot.testMessage("/help")
        assertMessage(1, "???")
    }

    class ContextA() : ChatContext
    class ContextB() : ChatContext

    @Test
    fun chatContextType() {
        val bot = chatBot(testClient) {
            use(chatContext)

            behaviour {
                onCommand("a") {
                    setContext(ContextA())
                }
                onCommand("b") {
                    setContext(ContextB())
                }

                into<ContextA> {
                    onMessage { sendMessage(message.chatId, "a") }
                }
                into<ContextB> {
                    onMessage { sendMessage(message.chatId, "b") }
                }
                onMessage { sendMessage(message.chatId, "empty") }
            }
        }

        bot.testMessage("test")
        assertMessage(1L, "empty")

        bot.testMessage("/a")
        bot.testMessage("test")
        assertMessage(1L, "a")

        bot.testMessage("/b")
        bot.testMessage("test")
        assertMessage(1L, "b")
    }

    data class SimpleContest(val key: String) : ChatContext

    @Test
    fun chatContextEquality() {
        val bot = chatBot(testClient) {
            use(chatContext)

            behaviour {
                onCommand("context") {
                    setContext(SimpleContest(message.text.substring("/context ".length)))
                }

                SimpleContest("1").into {
                    onMessage { sendMessage(message.chatId, "context 1") }
                }
                SimpleContest("2") into {
                    onMessage { sendMessage(message.chatId, "context 2") }
                }
                into<SimpleContest> {
                    onMessage { sendMessage(message.chatId, "context other") }
                }
                onMessage { sendMessage(message.chatId, "empty") }
            }
        }

        bot.testMessage("test")
        assertMessage(1L, "empty")

        bot.testMessage("/context 1")
        bot.testMessage("test")
        assertMessage(1L, "context 1")

        bot.testMessage("/context 2")
        bot.testMessage("test")
        assertMessage(1L, "context 2")

        bot.testMessage("/context 3")
        bot.testMessage("test")
        assertMessage(1L, "context other")
    }

    object AskNameContext : ChatContext
    class WithNameContext(val name: String) : ChatContext

    @Test
    fun userNameContext() {
        val bot = chatBot(testClient) {
            use(chatContext)

            behaviour {
                into<WithNameContext> {
                    onCommand("change_name") {
                        client.sendMessage(message.chatId, "Say your new name")
                        setContext(AskNameContext)
                    }

                    onMessage {
                        client.sendMessage(message.chatId, "Hello, ${this.context.name}!")
                    }
                }

                AskNameContext into {
                    onMessage {
                        client.sendMessage(message.chatId, "ok")
                        setContext(WithNameContext(message.text))
                    }
                }

                onCommand("start") {
                    client.sendMessage(message.chatId, "Hello! Say your name!")
                    setContext(AskNameContext)
                }
            }
        }

        bot.testMessage("/start")
        assertMessage(1L, "Hello! Say your name!")

        bot.testMessage("Kotlin Student")
        assertMessage(1L, "ok")

        bot.testMessage("/start")
        assertMessage(1L, "Hello, Kotlin Student!")

        bot.testMessage("test")
        assertMessage(1L, "Hello, Kotlin Student!")

        bot.testMessage("/change_name")
        assertMessage(1L, "Say your new name")

        bot.testMessage("New Name")
        assertMessage(1L, "ok")

        bot.testMessage("test")
        assertMessage(1L, "Hello, New Name!")

        bot.testMessage("/start")
        assertMessage(1L, "Hello, New Name!")
    }
}
