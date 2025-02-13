@JvmInline
value class A(val value: Int)

@JvmInline
value class B(val string: String)

class C(private val a: Int) {
    var number: Long? = null
    var text: String? = null

    fun setupNumber(number: Long) {
        this.number = number
    }

    fun setupText(string: String) {
        this.text = string
    }

    override fun toString(): String {
        return "C(a=$a, number=$number, text=$text)"
    }
}
