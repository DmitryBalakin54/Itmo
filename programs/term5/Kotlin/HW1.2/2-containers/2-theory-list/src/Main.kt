import java.util.*

fun constructors() {
    val arrayList: MutableList<Int> = ArrayList<Int>()
    val linkedList: MutableList<Int> = LinkedList<Int>()
}

fun fabrics() {
    val list: List<Int> = List(10) { index -> index }
    val mutableList: MutableList<Int> = MutableList(10) { index -> index }
}

fun builder() {
    val list = buildList<Int> {
        add(1)
        add(2)
        add(3)
    }
}

fun byValues() {
    val list: List<Int> = listOf(1, 2, 3)
    val mutableList: MutableList<Int> = mutableListOf(1, 2, 3)
}

fun mutableVsImmutable() {
    val mutableList: MutableList<Int> = listOf(1, 2, 3).toMutableList()
    val immutableList1: List<Int> = mutableListOf(1, 2, 3).toList()

    val immutableList2: List<Int> = mutableListOf(1, 2, 3) // upper cast
}

fun main() {
    constructors()
    fabrics()
    builder()
    byValues()
    mutableVsImmutable()
}
