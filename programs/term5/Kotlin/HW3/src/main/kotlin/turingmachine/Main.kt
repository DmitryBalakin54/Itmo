package turingmachine

import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.core.main
import com.github.ajalt.clikt.parameters.arguments.*
import com.github.ajalt.clikt.parameters.options.*
import com.github.ajalt.clikt.parameters.types.double
import com.github.ajalt.clikt.parameters.types.file
import java.io.File
import kotlinx.coroutines.delay
import kotlinx.coroutines.runBlocking

data class MachineConfig(
    val startingState: String,
    val acceptedState: String,
    val rejectedState: String,
    val blankSymbol: Char,
    val transitions: List<TransitionFunction>,
) {
    companion object {
        fun getMachine(filePath: File): MachineConfig {
            val lines = filePath.readLines()
            var startingState = ""
            var acceptedState = ""
            var rejectedState = ""
            var blankSymbol = BLANK
            val transitions = mutableListOf<TransitionFunction>()

            for (line in lines) {
                val parts = line.replace("\\s+".toRegex(), " ").trim().split(" ")
                when {
                    parts[0] == "start:" -> startingState = parts[1]
                    parts[0] == "accept:" -> acceptedState = parts[1]
                    parts[0] == "reject:" -> rejectedState = parts[1]
                    parts[0] == "blank:" -> blankSymbol = parts[1].single()
                    parts.size == 6 -> {
                        val state = parts[0]
                        var symbol = parts[1].single()
                        val newState = parts[3]
                        var newSymbol = parts[4].single()
                        val move = parts[5]

                        if (symbol == blankSymbol) {
                            symbol = BLANK
                        }

                        if (newSymbol == blankSymbol) {
                            newSymbol = BLANK
                        }

                        transitions.add(
                            TransitionFunction(
                                state,
                                symbol,
                                when (move) {
                                    ">" -> TapeTransition.Right
                                    "<" -> TapeTransition.Left
                                    "^" -> TapeTransition.Stay
                                    else -> error("Unknown move $move")
                                },
                                newSymbol,
                                newState,
                            ),
                        )
                    }
                }
            }

            if (acceptedState.isEmpty() || rejectedState.isEmpty() || startingState.isEmpty()) {
                error("Starting state, accepted state, rejected state must be contained")
            }

            return MachineConfig(startingState, acceptedState, rejectedState, blankSymbol, transitions)
        }
    }
}

class TuringMachineSimulator : CliktCommand() {
    private val BLANKS = BLANK.toString().repeat(2).toCharArray()
    private val machineFile: File by argument("machineFile", help = "Path to the Turing machine file")
        .file(mustExist = true, canBeFile = true, mustBeReadable = true)

    private val inputFile: File? by argument("inputFile", help = "Path to the input word file")
        .file(mustExist = true, canBeFile = true, mustBeReadable = true)
        .optional()
    private val auto by option("--auto", help = "Auto mode").flag(default = false)
    private val delay by option("--delay", help = "Delay in auto mode").double().default(0.5)

    override fun run() {
        val machineConfig = MachineConfig.getMachine(machineFile)
        val blankSymbol = machineConfig.blankSymbol
        val inputWord = inputFile?.let { it.readText().trim() } ?: readInputWord()

        val turingMachine = TuringMachine(
            machineConfig.startingState,
            machineConfig.acceptedState,
            machineConfig.rejectedState,
            machineConfig.transitions,
        )

        run(turingMachine, inputWord, blankSymbol, auto)
    }

    private fun readInputWord(): String {
        println("Enter the input word:")
        return readln().trim()
    }

    private fun run(turingMachine: TuringMachine, inputWord: String, blankSymbol: Char, isAuto: Boolean) = runBlocking {
        for (snapshot in turingMachine.simulate(inputWord)) {
            println(snapshot.toDisplayString(blankSymbol))
            if (isAuto) {
                println()
            }

            if (snapshot.state == turingMachine.getAcceptedState() ||
                snapshot.state == turingMachine.getRejectedState()
            ) {
                break
            }

            if (isAuto) {
                delay((delay * 1000).toLong())
            } else {
                println("Press Enter to continue...")
                readlnOrNull()
            }
        }
    }

    private fun TuringMachine.Snapshot.toDisplayString(blankSymbol: Char): String {
        val tapeContent = (BLANKS + tape.content + BLANKS)
        val positionMarker = tape.position
        val tapeString = tapeContent.mapIndexed { i, char ->
            if (i - BLANKS.size == positionMarker) "[$char]" else " $char "
        }.joinToString("")
        return "State: $state\nTape: ${tapeString.replace(BLANK, blankSymbol)}"
    }
}

fun main(args: Array<String>) = TuringMachineSimulator().main(args)
