import org.junit.Assert
import org.junit.Test
import kotlin.math.pow

class Tests {
    @Test
    fun testSimple() {
        val inputArray = intArrayOf(1, 2, 3)
        Assert.assertArrayEquals(intArrayOf(1, 4, 9), mapSquares(inputArray))
        Assert.assertArrayEquals(intArrayOf(1, 2, 3), inputArray)
    }

    @Test
    fun testEmpty() {
        Assert.assertArrayEquals(intArrayOf(), mapSquares(intArrayOf()))
    }

    @Test
    fun testBig() {
        val origin = IntArray(100)
        val input = IntArray(100)
        val output = IntArray(100)
        for (i in 0 until 100) {
            input[i] = (-1.0.pow(i).toInt() * i)
            origin[i] = input[i]
            output[i] = i * i
        }
        Assert.assertArrayEquals(output, mapSquares(input))
        Assert.assertArrayEquals(origin, input)
    }
}
