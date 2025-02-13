class IntMatrix(val rows: Int, val columns: Int) {

    private val matrix: IntArray
    init {
        require(rows > 0 && columns > 0) { "Rows and columns must be more than zero" }
        matrix = IntArray(rows * columns)
    }

    operator fun set(row: Int, column: Int, value: Int) {
        checkDim(row, column)
        matrix[index(row, column)] = value
    }

    operator fun get(row: Int, column: Int): Int {
        checkDim(row, column)
        return matrix[index(row, column)]
    }

    private fun index(row: Int, column: Int): Int = row * columns + column

    private fun checkDim(row: Int, column: Int) =
        require(row in 0..<rows && column in 0..<columns) {
            "Row must be in [0; $rows), column must be in [0; $columns)"
        }

}

fun main() {
    val matrix = IntMatrix(3, 4)
    println(matrix.rows)
    println(matrix.columns)
    println(matrix[0, 0])
    matrix[2, 3] = 42
    println(matrix[2, 3])
}
