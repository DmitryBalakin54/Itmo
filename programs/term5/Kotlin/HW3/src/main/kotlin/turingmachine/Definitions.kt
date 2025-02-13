package turingmachine

enum class TapeTransition {
    Left, Right, Stay
}

data class Transition(
    val newState: String,
    val newSymbol: Char,
    val move: TapeTransition,
)

data class TransitionFunction(
    val state: String,
    val symbol: Char,
    val transition: Transition,
) {
    constructor(state: String, symbol: Char, move: TapeTransition, newSymbol: Char, newState: String) :
        this(state, symbol, Transition(newState, newSymbol, move))

    constructor(state: String, symbol: Char, move: TapeTransition) :
        this(state, symbol, Transition(state, symbol, move))
}

val BLANK = 0.toChar()
