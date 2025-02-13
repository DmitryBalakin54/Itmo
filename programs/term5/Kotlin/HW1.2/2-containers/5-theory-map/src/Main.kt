import java.util.*

fun constructors() {
    val hashMap: MutableMap<Int, String> = HashMap<Int, String>()
    val linkedMap: MutableMap<Int, String> = TreeMap<Int, String>()
}

fun byValues() {
    val map: Map<Int, String> = mapOf(1 to "1", 2 to "2")
    val mutableMap: MutableMap<Int, String> = mutableMapOf(1 to "1", 2 to "2")
}

fun builder() {
    val map: Map<Int, String> = buildMap {
        put(1, "1")
    }
}

fun mutableVsImmutable() {
    val mutableMap: MutableMap<Int, String> = mapOf(1 to "1", 2 to "2").toMutableMap()
    val immutableMap1: Map<Int, String> = mutableMapOf(1 to "1", 2 to "2").toMap()

    val immutableMap2: Map<Int, String> = mutableMapOf(1 to "1", 2 to "2") // upper cast
}

fun main() {
    constructors()
    byValues()
    builder()
    mutableVsImmutable()
}
