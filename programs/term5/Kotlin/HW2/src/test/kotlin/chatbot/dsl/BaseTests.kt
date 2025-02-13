package chatbot.dsl

import chatbot.api.*
import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.BeforeEach

abstract class BaseTests {
    protected var testClient = TestClient()
    protected var chatContext = InMemoryContextsManager()

    @BeforeEach
    fun before() {
        testClient = TestClient()
        chatContext = InMemoryContextsManager()
    }

    protected fun ChatBot.testMessage(text: String) {
        processMessages(
            Message(
                id = 1,
                chatId = ChatId(1),
                text = text,
            ),
        )
    }

    protected fun assertMessage(
        expectedCharId: Long,
        expectedText: String,
        expectedKeyboard: Keyboard? = null,
        expectedReplyMessageId: MessageId? = null,
    ) {
        val messages = testClient.pullMessages()
        Assertions.assertEquals(1, messages.size)

        val message = messages.single()
        Assertions.assertEquals(ChatId(expectedCharId), message.chatId)
        Assertions.assertEquals(expectedText, message.text)
        Assertions.assertEquals(expectedKeyboard, message.keyboard)
        Assertions.assertEquals(expectedReplyMessageId, message.replyMessageId)
    }

    protected fun assertNoMessages() {
        val messages = testClient.pullMessages()
        Assertions.assertTrue(messages.isEmpty())
    }

    companion object {
        class TestClient : Client {
            data class SendMessageInfo(
                val chatId: ChatId,
                val text: String,
                val keyboard: Keyboard?,
                val replyMessageId: MessageId?,
            )

            private val messages = mutableListOf<SendMessageInfo>()

            fun pullMessages(): List<SendMessageInfo> {
                val messages = this.messages.toList()
                this.messages.clear()
                return messages
            }

            override fun sendMessage(chatId: ChatId, text: String, keyboard: Keyboard?, replyMessageId: MessageId?) {
                messages.add(SendMessageInfo(chatId, text, keyboard, replyMessageId))
            }
        }

        class InMemoryContextsManager : ChatContextsManager {
            private val data = mutableMapOf<ChatId, ChatContext?>()

            override fun getContext(chatId: ChatId): ChatContext? {
                return data[chatId]
            }

            override fun setContext(chatId: ChatId, newState: ChatContext?) {
                data[chatId] = newState
            }
        }

        val TEST_MESSAGE_1 = Message(
            id = 123L,
            chatId = ChatId(42),
            text = "hello, bot",
        )
        val TEST_MESSAGE_HELP = Message(
            id = 124L,
            chatId = ChatId(11),
            text = "/help me",
        )
    }
}
