import org.junit.Assert
import org.junit.FixMethodOrder
import org.junit.Test
import org.junit.runners.MethodSorters

@FixMethodOrder(MethodSorters.NAME_ASCENDING)
class Tests {
    @Test
    fun testConstructors() {
        IntMatrix(5, 12)
        assertThrows<IllegalArgumentException> { IntMatrix(-1, 1) }
        assertThrows<IllegalArgumentException> { IntMatrix(1, -1) }
        assertThrows<IllegalArgumentException> { IntMatrix(-1, -1) }
    }

    @Test
    fun testMatrixInstance() {
        val matrix = IntMatrix(rows = 10, columns = 20)
        Assert.assertEquals(10, matrix.rows)
        Assert.assertEquals(20, matrix.columns)

        matrix[0, 0] = 12
        Assert.assertEquals(12, matrix[0, 0])

        assertThrows<IllegalArgumentException> { matrix[-1, 0] }
        assertThrows<IllegalArgumentException> { matrix[0, -1] }
        assertThrows<IllegalArgumentException> { matrix[20, 0] }
        assertThrows<IllegalArgumentException> { matrix[0, 30] }

        assertThrows<IllegalArgumentException> { matrix[-1, 0] = 10 }
        assertThrows<IllegalArgumentException> { matrix[0, -1] = 10 }
        assertThrows<IllegalArgumentException> { matrix[20, 0] = 10 }
        assertThrows<IllegalArgumentException> { matrix[0, 30] = 10 }
    }

    @Test
    fun testMatrixValues() {
        val matrix = IntMatrix(rows = 5, columns = 10)
        for (i in 0 until 5) {
            for (j in 0 until 10) {
                matrix[i, j] = i * 10 + j
            }
        }
        for (j in 0 until 10) {
            for (i in 0 until 5) {
                Assert.assertEquals(i * 10 + j, matrix[i, j])
            }
        }
    }
}
