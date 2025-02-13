package turingmachine

import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.MethodOrderer
import org.junit.jupiter.api.Order
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.TestMethodOrder

@TestMethodOrder(MethodOrderer.OrderAnnotation::class)
class TapeTests {
    @Test
    @Order(1)
    fun initializeTape() {
        val tape = TuringMachine.Tape("hello")
        assertTapeEquals("hello", 0, tape, "incorrect tape from string hello")
    }

    @Test
    @Order(2)
    fun applyTransitionStay() {
        var tape = TuringMachine.Tape("hello")
        tape = tape.applyTransition('a', TapeTransition.Stay)
        assertTapeEquals("aello", 0, tape, "incorrect modify tape TapeTransition.Stay")
        tape = tape.applyTransition('h', TapeTransition.Stay)
        assertTapeEquals("hello", 0, tape, "incorrect modify tape TapeTransition.Stay")
    }

    @Test
    @Order(3)
    fun applyTransitionRight() {
        var tape = TuringMachine.Tape("cat")
        tape = tape.applyTransition('c', TapeTransition.Right)
        assertTapeEquals("cat", 1, tape, "incorrect modify tape TapeTransition.Right")
        tape = tape.applyTransition('a', TapeTransition.Right)
        assertTapeEquals("cat", 2, tape, "incorrect modify tape TapeTransition.Right")
        tape = tape.applyTransition('t', TapeTransition.Right)
        assertTapeEquals("cat$BLANK", 3, tape, "incorrect modify tape TapeTransition.Right")
        tape = tape.applyTransition('2', TapeTransition.Stay)
        assertTapeEquals("cat2", 3, tape, "incorrect modify tape TapeTransition.Stay and write new char")
    }

    @Test
    @Order(4)
    fun applyTransitionLeft() {
        var tape = TuringMachine.Tape("cat")
        tape = tape.applyTransition('c', TapeTransition.Right)
        assertTapeEquals("cat", 1, tape, "incorrect modify tape TapeTransition.Right")
        tape = tape.applyTransition('a', TapeTransition.Right)
        assertTapeEquals("cat", 2, tape, "incorrect modify tape TapeTransition.Right")
        tape = tape.applyTransition('g', TapeTransition.Left)
        assertTapeEquals("cag", 1, tape, "incorrect modify tape TapeTransition.Left")
        tape = tape.applyTransition('o', TapeTransition.Left)
        assertTapeEquals("cog", 0, tape, "incorrect modify tape TapeTransition.Left")
        tape = tape.applyTransition('d', TapeTransition.Left)
        assertTapeEquals(BLANK + "dog", 0, tape, "incorrect modify tape TapeTransition.Left")
        tape = tape.applyTransition('(', TapeTransition.Stay)
        assertTapeEquals("(dog", 0, tape, "incorrect modify tape TapeTransition.Stay and write new char")
    }

    @Test
    @Order(5)
    fun blankLeft() {
        var tape = TuringMachine.Tape("cat")
        tape = tape.applyTransition('c', TapeTransition.Left)
        assertTapeEquals(BLANK + "cat", 0, tape, "incorrect modify tape using TapeTransition.Left")
        tape = tape.applyTransition('2', TapeTransition.Left)
        assertTapeEquals(
            BLANK + "2cat",
            0,
            tape,
            "incorrect modify tape using TapeTransition.Left and write char to blank position",
        )
        tape = tape.applyTransition('0', TapeTransition.Left)
        assertTapeEquals(
            BLANK + "02cat",
            0,
            tape,
            "incorrect modify tape using TapeTransition.Left and write char to blank position",
        )
        tape = tape.applyTransition('1', TapeTransition.Right)
        assertTapeEquals(
            "102cat",
            1,
            tape,
            "incorrect modify tape using TapeTransition.Right and write char to blank position",
        )
        tape = tape.applyTransition('1', TapeTransition.Left)
        assertTapeEquals(
            "112cat",
            0,
            tape,
            "incorrect modify tape using TapeTransition.Left and write new char",
        )
        tape = tape.applyTransition(BLANK, TapeTransition.Stay)
        assertTapeEquals(
            BLANK + "12cat",
            0,
            tape,
            "incorrect modify tape using TapeTransition.Left and write blank char",
        )
        tape = tape.applyTransition(BLANK, TapeTransition.Right)
        assertTapeEquals("12cat", 0, tape, "incorrect modify, blank prefix should trimmed")
    }

    @Test
    @Order(10)
    fun copyMethod() {
        var tape = TuringMachine.Tape("capybara")
        val capyTape = tape.copy()
        assertTapeEquals("capybara", 0, capyTape, "copy operation should create copy")

        tape = tape.applyTransition('d', TapeTransition.Stay)
        val dapyTape = tape.copy()
        assertTapeEquals("capybara", 0, capyTape, "copy operation should create independent copy")
        assertTapeEquals("dapybara", 0, dapyTape)
        assertTapeEquals("dapybara", 0, tape)

        tape = tape.applyTransition('d', TapeTransition.Right)
        val dapyTape2 = tape.copy()
        assertTapeEquals("capybara", 0, capyTape, "copy operation should create independent copy")
        assertTapeEquals("dapybara", 0, dapyTape, "copy operation should create independent copy")
        assertTapeEquals("dapybara", 1, dapyTape2, "copy operation should create independent copy")
        assertTapeEquals("dapybara", 1, tape)
    }

    @Test
    @Order(21)
    fun equalsMethod() {
        val tape1 = TuringMachine.Tape("cotpizza")
            .applyTransition('c', TapeTransition.Right)
            .applyTransition('a', TapeTransition.Right)
            .applyTransition('t', TapeTransition.Right)

        val tape2 = TuringMachine.Tape("coolpizza")
            .applyTransition(BLANK, TapeTransition.Right)
            .applyTransition('c', TapeTransition.Right)
            .applyTransition('a', TapeTransition.Right)
            .applyTransition('t', TapeTransition.Right)

        Assertions.assertEquals(tape1, tape2, "tape with equal content and positions should be equal")

        val tape21 = tape2.copy().applyTransition('p', TapeTransition.Right)
        Assertions.assertNotEquals(tape1, tape21, "tape with equal content but different positions should be not equal")

        val tape22 = tape2.copy().applyTransition('l', TapeTransition.Stay)
        Assertions.assertNotEquals(tape1, tape22, "tape with equal position but different content should be not equal")
    }

    @Test
    @Order(31)
    fun emptyTapeInput() {
        val x = TuringMachine.Tape("")
        assertTapeEquals(BLANK.toString(), 0, x)

        val x2 = x.applyTransition(BLANK, TapeTransition.Right)
        assertTapeEquals(BLANK.toString(), 0, x2)

        val x3 = x2.applyTransition(BLANK, TapeTransition.Right)
        assertTapeEquals(BLANK.toString(), 0, x3)

        val x4 = x3.applyTransition('o', TapeTransition.Left)
        assertTapeEquals("${BLANK}o", 0, x4)

        val x5 = x4.applyTransition('g', TapeTransition.Right)
        assertTapeEquals("go", 1, x5)
    }

    @Test
    @Order(41)
    fun toStringMethodNotDefault() {
        val x = TuringMachine.Tape("")

        Assertions.assertNotEquals(
            "turingmachine.TuringMachine\$Tape@${x.hashCode().toString(16)}",
            x.toString(),
            "toString should be implemented",
        )
    }

    companion object {
        fun assertTapeEquals(
            expectedContent: String,
            expectedPosition: Int,
            actual: TuringMachine.Tape,
            message: String? = null,
        ) {
            assertTapeEquals(expectedContent.toCharArray(), expectedPosition, actual, message)
        }

        fun assertTapeEquals(
            expectedContent: CharArray,
            expectedPosition: Int,
            actual: TuringMachine.Tape,
            message: String? = null,
        ) {
            Assertions.assertEquals(expectedPosition, actual.position, message?.let { "$it: position" })
            Assertions.assertEquals(
                expectedContent.joinToString(),
                actual.content.joinToString(),
                message?.let { "$it: content" },
            )
        }
    }
}
