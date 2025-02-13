package turingmachine

import kotlin.math.max
import kotlin.math.min

class TuringMachine(
    private val startingState: String,
    private val acceptedState: String,
    private val rejectedState: String,
    transitions: Collection<TransitionFunction>,
) {
    fun getAcceptedState(): String {
        return acceptedState
    }

    fun getRejectedState(): String {
        return rejectedState
    }

    private val transitionMap: Map<Pair<String, Char>, Transition> = transitions
        .associate { (state, symbol, transition) -> Pair(state, symbol) to transition }

    fun initialSnapshot(input: String): Snapshot {
        return Snapshot(startingState, Tape(input))
    }

    fun simulateStep(snapshot: Snapshot): Snapshot {
        val currentSymbol = snapshot.tape.getCurrentChar()
        val transition = transitionMap[snapshot.state to currentSymbol]

        return if (transition != null) {
            snapshot.applyTransition(transition)
        } else {
            Snapshot(rejectedState, snapshot.tape)
        }
    }

    fun simulate(initialString: String): Sequence<Snapshot> = sequence {
        var currentSnapshot = initialSnapshot(initialString)

        while (currentSnapshot.state != acceptedState && currentSnapshot.state != rejectedState) {
            yield(currentSnapshot)
            currentSnapshot = simulateStep(currentSnapshot)
        }

        yield(currentSnapshot)
    }

    class Snapshot(val state: String, val tape: Tape) {

        fun applyTransition(transition: Transition): Snapshot {
            return Snapshot(transition.newState, tape.applyTransition(transition.newSymbol, transition.move))
        }

        fun copy(): Snapshot {
            return Snapshot(state, tape.copy())
        }

        override fun equals(other: Any?): Boolean {
            if (this === other) {
                return true
            }

            if (other !is Snapshot) {
                return false
            }

            return state == other.state && tape == other.tape
        }

        override fun toString(): String {
            return "{SNAPSHOT $state, $tape}"
        }

        override fun hashCode(): Int {
            var result = state.hashCode()
            result = 11 * result + tape.hashCode()
            return result
        }
    }

    class Tape(str: String) {
        private val tapeContent: MutableList<Char> = str.toMutableList()
        private var tapePosition: Int = 0

        private var firstNonBlankIndex: Int = -1
        private var lastNonBlankIndex: Int = -1

        val content: CharArray
            get() {
                return if (tapeContent.isEmpty() || firstNonBlankIndex == -1) {
                    charArrayOf(BLANK)
                } else {
                    tapeContent.subList(
                        min(firstNonBlankIndex, tapePosition),
                        max(lastNonBlankIndex + 1, tapePosition + 1),
                    ).toCharArray()
                }
            }

        fun getCurrentChar(): Char {
            if (tapeContent.isEmpty()) {
                return BLANK
            }

            return tapeContent[tapePosition]
        }

        val position: Int
            get() {
                return if (firstNonBlankIndex == -1) 0 else max(tapePosition - firstNonBlankIndex, 0)
            }

        init {
            firstNonBlankIndex = tapeContent.indexOfFirst { it != BLANK }
            lastNonBlankIndex = tapeContent.indexOfLast { it != BLANK }
        }

        fun applyTransition(char: Char, move: TapeTransition): Tape {
            if (char != BLANK && tapeContent.isEmpty()) {
                firstNonBlankIndex = tapePosition
                lastNonBlankIndex = tapePosition
            }

            if (tapeContent.isEmpty()) {
                tapeContent.add(BLANK)
            }

            tapeContent[tapePosition] = char

            when (move) {
                TapeTransition.Left -> {
                    if (tapePosition == 0) {
                        tapeContent.add(0, BLANK)
                        firstNonBlankIndex++
                        lastNonBlankIndex++
                    } else {
                        tapePosition--
                    }

                    if (firstNonBlankIndex == -1 && lastNonBlankIndex == -1) {
                        firstNonBlankIndex = tapePosition + 1
                        lastNonBlankIndex = tapePosition + 1
                    }
                }
                TapeTransition.Right -> {
                    tapePosition++
                    if (tapePosition >= tapeContent.size) {
                        tapeContent.add(BLANK)
                    }

                    if (tapeContent[tapePosition - 1] == BLANK && firstNonBlankIndex != -1) {
                        firstNonBlankIndex++
                    }
                }
                TapeTransition.Stay -> {}
            }

            if (char != BLANK) {
                if (firstNonBlankIndex == -1 || tapePosition < firstNonBlankIndex) {
                    firstNonBlankIndex = tapePosition
                }
            }

            return this
        }

        fun copy(): Tape {
            val tape = Tape("")
            tape.tapePosition = tapePosition
            tape.tapeContent.addAll(tapeContent)
            tape.firstNonBlankIndex = firstNonBlankIndex
            tape.lastNonBlankIndex = lastNonBlankIndex
            return tape
        }

        override fun equals(other: Any?): Boolean {
            if (this === other) {
                return true
            }

            if (other !is Tape) {
                return false
            }

            return content.contentEquals(other.content) && position == other.position
        }

        override fun toString(): String {
            return "{TAPE ${tapeContent.joinToString("").replace(BLANK, '_')}}"
        }

        override fun hashCode(): Int {
            var result = tapeContent.hashCode()
            result = 31 * result + tapePosition
            result = 22 * result + firstNonBlankIndex
            result = 15 * result + lastNonBlankIndex
            return result
        }
    }
}
