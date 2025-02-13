import java.io.InputStream
import kotlin.properties.ReadOnlyProperty
import kotlin.reflect.KProperty

class Config(fileName: String) {
    private val map: Map<String, String> = extractContent(fileName)
    companion object {
        private fun extractContent(fileName: String): Map<String, String> {
            val stream = requireNotNull(getResource(fileName)) { "Resource is null, fileName=$fileName" }

            val map: MutableMap<String, String> = mutableMapOf()

            stream.reader().forEachLine { line ->
                val values = line.split('=').map { str -> str.trim() }
                require(values.size == 2 && values.all { vl -> vl.isNotEmpty() }) { "Incorrect line $line" }
                val (key, vl) = values
                map[key] = vl
            }

            return map
        }
    }

    operator fun provideDelegate(thisRef: Any?, property: KProperty<*>): ReadOnlyProperty<Any?, String> {
        val key = property.name
        val value = requireNotNull(map[key]) { "Incorrect key, key=${property.name}" }

        return ReadOnlyProperty { _, _ -> value }
    }

}

@Suppress(
    "RedundantNullableReturnType",
    "UNUSED_PARAMETER",
)
fun getResource(fileName: String): InputStream? {
    // do not touch this function
    val content =
        """
        |valueKey = 10
        |otherValueKey = stringValue 
        """.trimMargin()

    return content.byteInputStream()
}
