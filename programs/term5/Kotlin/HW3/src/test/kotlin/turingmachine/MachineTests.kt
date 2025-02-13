package turingmachine

import org.junit.jupiter.api.*
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.ValueSource
import turingmachine.TapeTests.Companion.assertTapeEquals

@TestMethodOrder(MethodOrderer.OrderAnnotation::class)
class MachineTests {
    @Test
    @Order(1)
    fun initialSnapshot() {
        val s1 = INV_MACHINE.initialSnapshot("aba")
        Assertions.assertEquals(TuringMachine.Snapshot("q0", TuringMachine.Tape("aba")), s1)

        val s2 = INV_MACHINE.initialSnapshot("aaaaa")
        Assertions.assertEquals(TuringMachine.Snapshot("q0", TuringMachine.Tape("aaaaa")), s2)
    }

    @Test
    @Order(11)
    fun simulateByStep() {
        val s1 = INV_MACHINE.simulateStep(INV_MACHINE.initialSnapshot("aba"))
        Assertions.assertEquals("q0", s1.state, "step 1: state")
        assertTapeEquals("bba", 1, s1.tape, "step 1: tape")

        val s2 = INV_MACHINE.simulateStep(s1)
        Assertions.assertEquals("q0", s2.state, "step 2: state")
        assertTapeEquals("baa", 2, s2.tape, "step 2: tape")

        val s3 = INV_MACHINE.simulateStep(s2)
        Assertions.assertEquals("q0", s3.state, "step 3: state")
        assertTapeEquals("bab$BLANK", 3, s3.tape, "step 3: tape")

        val s4 = INV_MACHINE.simulateStep(s3)
        Assertions.assertEquals("ac", s4.state, "step 4: state")
        assertTapeEquals("bab", 2, s4.tape, "step 4: tape")
    }

    @Test
    @Order(15)
    fun simulateByStepRejected() {
        val s0 = NO_ONES_MACHINE.initialSnapshot("0111")
        val s1 = NO_ONES_MACHINE.simulateStep(s0)
        val s2 = NO_ONES_MACHINE.simulateStep(s1)

        Assertions.assertEquals("rj", s2.state)
    }

    @Test
    @Order(16)
    fun simulateByStepRejectedIncorrectSymbol() {
        val s0 = INV_MACHINE.initialSnapshot("a1a")
        val s1 = INV_MACHINE.simulateStep(s0)

        val s2 = INV_MACHINE.simulateStep(s1)
        Assertions.assertEquals("rj", s2.state, "step 2: state should be rejected on unexpected symbol and state")
        assertTapeEquals("b1a", 1, s2.tape, "step 2: tape should not be changed on unexpected symbol and state")
    }

    @ParameterizedTest
    @ValueSource(strings = ["", "0", "1", "11101"])
    @Order(21)
    fun infinityMachine(input: String) {
        val steps = 100

        val snapshots = APPEND_ZERO.simulate(input)
            .map { it.copy() }
            .take(1 + steps)
            .toList()

        for (i in snapshots.indices) {
            val expectedTape = buildString {
                append(input)
                if (i > input.length) {
                    append("0".repeat(i - input.length))
                    append(BLANK)
                }
            }
            if (i < input.length) {
                Assertions.assertEquals(expectedTape, snapshots[i].tape.content.joinToString(""))
            } else {
                Assertions.assertEquals(
                    input + "0".repeat(i - input.length) + BLANK,
                    snapshots[i].tape.content.joinToString(""),
                )
            }
        }
    }

    @ParameterizedTest
    @ValueSource(strings = ["", "0", "00", "000000000", "0001001", "111"])
    @Order(101)
    fun noOnesMachine(input: String) {
        val finalState = NO_ONES_MACHINE.simulate(input).last()
        if (input.contains("1")) {
            Assertions.assertEquals(
                "rj",
                finalState.state,
                "machine should reject on input $input (with 1 in input)",
            )
        } else {
            Assertions.assertEquals(
                "ac",
                finalState.state,
                "machine should accept on input $input (without 1 in input)",
            )
        }
        Assertions.assertEquals(
            input.takeIf { it.isNotEmpty() } ?: (BLANK + ""),
            finalState.tape.content.joinToString(""),
            "machine should not change tape value",
        )
    }

    @ParameterizedTest
    @ValueSource(ints = [0, 1, 2, 3, 6, 7, 8, 21, Int.MAX_VALUE])
    @Order(102)
    fun incrementMachine(number: Int) {
        val input = number.toString(2)
        val finalState = INC_MACHINE.simulate(input).last()
        Assertions.assertEquals(
            "ac",
            finalState.state,
            "machine should accept on input $input (without 1 in input)",
        )

        Assertions.assertEquals(
            (number.toLong() + 1).toString(2),
            finalState.tape.content.joinToString(""),
            "machine should not change tape value",
        )
    }
}

val INV_MACHINE = TuringMachine(
    "q0",
    "ac",
    "rj",
    listOf(
        TransitionFunction("q0", 'a', TapeTransition.Right, 'b', "q0"),
        TransitionFunction("q0", 'b', TapeTransition.Right, 'a', "q0"),
        TransitionFunction("q0", BLANK, TapeTransition.Left, BLANK, "ac"),
    ),
)

val NO_ONES_MACHINE = TuringMachine(
    "q0",
    "ac",
    "rj",
    listOf(
        TransitionFunction("q0", '0', TapeTransition.Right, '0', "q0"),
        TransitionFunction("q0", '1', TapeTransition.Stay, '1', "rj"),
        TransitionFunction("q0", BLANK, TapeTransition.Left, BLANK, "ac"),
    ),
)

val INC_MACHINE = TuringMachine(
    "q0",
    "ac",
    "rj",
    listOf(
        TransitionFunction("q0", '0', TapeTransition.Right),
        TransitionFunction("q0", '1', TapeTransition.Right),
        TransitionFunction("q0", BLANK, TapeTransition.Left, BLANK, "q1"),
        TransitionFunction("q1", '0', TapeTransition.Stay, '1', "q2"),
        TransitionFunction("q1", '1', TapeTransition.Left, '0', "q1"),
        TransitionFunction("q1", BLANK, TapeTransition.Stay, '1', "ac"),
        TransitionFunction("q2", '0', TapeTransition.Left),
        TransitionFunction("q2", '1', TapeTransition.Left),
        TransitionFunction("q2", BLANK, TapeTransition.Right, BLANK, "ac"),
    ),
)

val APPEND_ZERO = TuringMachine(
    "q0",
    "!",
    "rejected",
    listOf(
        TransitionFunction("q0", '0', TapeTransition.Right, '0', "q0"),
        TransitionFunction("q0", '1', TapeTransition.Right, '1', "q0"),
        TransitionFunction("q0", BLANK, TapeTransition.Right, '0', "q0"),
    ),
)
