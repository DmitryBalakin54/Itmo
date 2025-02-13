import java.util.*

fun constructors() {
    val hashSet: MutableSet<Int> = HashSet<Int>()
    val treeSet: MutableSet<Int> = TreeSet<Int>()
}

fun builder() {
    val set: Set<Int> = buildSet {
        add(1)
        add(2)
        add(3)
    }
}

fun byValues() {
    val set: Set<Int> = setOf(1, 2, 3)
    val mutableSet: MutableSet<Int> = mutableSetOf(1, 2, 3)
}

fun mutableVsImmutable() {
    val mutableSet: MutableSet<Int> = setOf(1, 2, 3).toMutableSet()
    val immutableSet1: Set<Int> = mutableSetOf(1, 2, 3).toSet()

    val immutableSet2: Set<Int> = mutableSetOf(1, 2, 3) // upper cast
}

fun main() {
    constructors()
    builder()
    byValues()
    mutableVsImmutable()
}
