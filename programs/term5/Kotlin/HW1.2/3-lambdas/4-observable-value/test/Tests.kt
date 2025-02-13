import org.junit.Assert
import org.junit.Test

class Test {
    @Test
    fun testConstruct() {
        val mutableValue: MutableValue<String> = MutableValue("initial")
        Assert.assertEquals("initial", mutableValue.value)

        mutableValue.value = "updated"
        Assert.assertEquals("updated", mutableValue.value)
    }

    @Test
    fun testConstructAndUseAsImmutable() {
        val value: Value<String> = MutableValue("initial")
        Assert.assertEquals("initial", value.value)

        val cancellation = value.observe { Assert.assertEquals("initial", it) }
        cancellation.cancel()
    }

    @Test
    fun testObserve() {
        val observer = Observer()

        val mutableValue = MutableValue("initial")

        val cancellation = mutableValue.observe(observer::internalValue::set)
        Assert.assertEquals("initial", observer.internalValue)

        mutableValue.value = "updated"
        Assert.assertEquals("updated", observer.internalValue)

        cancellation.cancel()
        mutableValue.value = "final"
        Assert.assertEquals("updated", observer.internalValue)
    }

    @Test
    fun testObserveSerial() {
        val observerA = Observer()
        val observerB = Observer()

        val value = MutableValue("initial")

        val cancelA = value.observe(observerA::internalValue::set)
        Assert.assertEquals("initial", observerA.internalValue)

        val cancelB = value.observe(observerB::internalValue::set)
        Assert.assertEquals("initial", observerB.internalValue)

        value.value = "updated"
        Assert.assertEquals("updated", observerA.internalValue)
        Assert.assertEquals("updated", observerB.internalValue)

        cancelA.cancel()
        value.value = "cancel A"
        Assert.assertEquals("updated", observerA.internalValue)
        Assert.assertEquals("cancel A", observerB.internalValue)

        cancelB.cancel()
        value.value = "cancel B"
        Assert.assertEquals("updated", observerA.internalValue)
        Assert.assertEquals("cancel A", observerB.internalValue)
    }

    private class Observer {
        var internalValue: String? = null
    }
}
