package turingmachine

import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.MethodOrderer
import org.junit.jupiter.api.Order
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.TestMethodOrder
import turingmachine.TapeTests.Companion.assertTapeEquals

@TestMethodOrder(MethodOrderer.OrderAnnotation::class)
class SnapshotTests {

    @Test
    @Order(1)
    fun testConstructor() {
        TuringMachine.Snapshot("q0", TuringMachine.Tape("hello"))
    }

    @Test
    @Order(2)
    fun testEquals() {
        val snapshot1 = TuringMachine.Snapshot("q0", TuringMachine.Tape("hello"))

        val tape2 = TuringMachine.Tape("oello")
            .applyTransition('h', TapeTransition.Stay)
        val snapshot2 = TuringMachine.Snapshot("q0", tape2)

        Assertions.assertEquals(snapshot1, snapshot2, "snapshot with equal tapes and state should be equal")

        Assertions.assertNotEquals(
            snapshot1,
            TuringMachine.Snapshot("q1", tape2),
            "snapshot with equal tape but different state should be not equal",
        )
        Assertions.assertNotEquals(
            snapshot1,
            TuringMachine.Snapshot("q2", TuringMachine.Tape("hello")),
            "snapshot with equal state but different tape should be not equal",
        )
    }

    @Test
    @Order(3)
    fun toStringMethodNotDefault() {
        val x = TuringMachine.Snapshot("", TuringMachine.Tape(""))

        Assertions.assertNotEquals(
            "turingmachine.TuringMachine\$Snapshot@${x.hashCode().toString(16)}",
            x.toString(),
            "toString should be implemented",
        )
    }

    @Test
    @Order(21)
    fun applyTransition() {
        var s = TuringMachine.Snapshot("q0", TuringMachine.Tape("line"))

        s = s.applyTransition(Transition("q0", 'c', TapeTransition.Right))
        Assertions.assertEquals("q0", s.state, "state after applyTransition should be correct")
        assertTapeEquals("cine", 1, s.tape, "tape after applyTransition should be correct")

        s = s.applyTransition(Transition("q1", 'o', TapeTransition.Right))
        Assertions.assertEquals("q1", s.state, "state after applyTransition should be correct")
        assertTapeEquals("cone", 2, s.tape, "tape after applyTransition should be correct")

        s = s.applyTransition(Transition("q0", 'd', TapeTransition.Stay))
        Assertions.assertEquals("q0", s.state, "state after applyTransition should be correct")
        assertTapeEquals("code", 2, s.tape, "tape after applyTransition should be correct")

        s = s.applyTransition(Transition("finish", 'd', TapeTransition.Left))
        Assertions.assertEquals("finish", s.state, "state after applyTransition should be correct")
        assertTapeEquals("code", 1, s.tape, "tape after applyTransition should be correct")

        val tapeAfter = TuringMachine.Tape("code").applyTransition('c', TapeTransition.Right)
        Assertions.assertEquals(
            TuringMachine.Snapshot("finish", tapeAfter),
            s,
            "snapshot after some modifications should be equal to expected snapshot",
        )
    }

    @Test
    @Order(31)
    fun copyMethod() {
        var s = TuringMachine.Snapshot("q0", TuringMachine.Tape("line"))
        val s0 = s.copy()
        Assertions.assertEquals(s, s0)
        Assertions.assertEquals("q0", s0.state, "state of snapshot should be correct")
        Assertions.assertEquals("q0", s.state, "state of snapshot should be correct")
        assertTapeEquals("line", 0, s0.tape, "tape in snapshot should be correct")
        assertTapeEquals("line", 0, s.tape, "tape in snapshot should be correct")

        s = s.applyTransition(Transition("q0", 'L', TapeTransition.Left))
        val s1 = s.copy()
        Assertions.assertEquals("q0", s0.state, "state of snapshot should be correct")
        Assertions.assertEquals("q0", s.state, "state of snapshot should be correct")
        Assertions.assertEquals("q0", s1.state, "state of snapshot should be correct")
        assertTapeEquals("line", 0, s0.tape, "tape in snapshot should be correct")
        assertTapeEquals(BLANK + "Line", 0, s.tape, "tape in snapshot should be correct")
        assertTapeEquals(BLANK + "Line", 0, s1.tape, "tape in snapshot should be correct")

        s = s.applyTransition(Transition("q1", 'q', TapeTransition.Right))
        Assertions.assertEquals("q0", s0.state, "state of snapshot should be correct")
        Assertions.assertEquals("q0", s1.state, "state of snapshot should be correct")
        Assertions.assertEquals("q1", s.state, "state of snapshot should be correct")
        assertTapeEquals("line", 0, s0.tape, "tape in snapshot should be correct")
        assertTapeEquals(BLANK + "Line", 0, s1.tape, "tape in snapshot should be correct")
        assertTapeEquals("qLine", 1, s.tape, "tape in snapshot should be correct")
    }
}
