fun constructor() {
    val array: Array<String> = Array<String>(10) { index -> "$index" }

    val inArray1: IntArray = IntArray(10)
    val inArray2: IntArray = IntArray(10) { index -> index }
}

fun byValues() {
    val stringArray: Array<String> = arrayOf<String>("1", "2", "3")
    val intArray: IntArray = intArrayOf(1, 2, 3)
}

fun main() {
    constructor()
    byValues()
}
