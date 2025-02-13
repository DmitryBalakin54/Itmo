import org.junit.Assert
import org.junit.Test

class Tests {
    @Test
    fun testTask1() {
        val processor: (A) -> B = { B("${it.value}") }
        val default = B("i am null")

        Assert.assertEquals(B("1"), task1(A(1), default, processor))
        Assert.assertEquals(B("i am null"), task1(null, default, processor))
    }

    @Test
    fun testTask2() {
        var biFunctionResult: Pair<Int, String>? = null
        val modifier: (A) -> A = { a -> A(a.value * 10) }

        fun biFunction(a: A, s: String): Long {
            biFunctionResult = a.value to s
            return Long.MAX_VALUE
        }

        Assert.assertEquals(A(20), task2(A(2), modifier, ::biFunction))
        Assert.assertEquals(20 to "additional", biFunctionResult)
    }

    @Test
    fun testTask3() {
        val constructor: (A) -> C = { a -> C(a.value) }
        val compressor: C.(Boolean) -> B = { compress -> B(toString()) }
        Assert.assertEquals(B("C(a=3, number=28, text=Hello)"), task3(A(3), constructor, compressor))
        Assert.assertEquals(B("C(a=333, number=28, text=Hello)"), task3(A(333), constructor, compressor))
    }
}
