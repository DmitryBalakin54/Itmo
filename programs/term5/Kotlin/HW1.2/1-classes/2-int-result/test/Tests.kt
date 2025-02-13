import org.junit.Assert
import org.junit.FixMethodOrder
import org.junit.Test
import org.junit.runners.MethodSorters

@FixMethodOrder(MethodSorters.NAME_ASCENDING)
class Tests {
    @Test
    fun test1CrateOk() {
        val result = IntResult.Ok(value = 10)
        @Suppress("USELESS_IS_CHECK")
        Assert.assertTrue("OK result should be instance of IntResult", result is IntResult)
        Assert.assertEquals(10, result.value)
    }

    @Test
    fun test2CrateError() {
        val result = IntResult.Error(reason = "Something is going wrong")
        @Suppress("USELESS_IS_CHECK")
        Assert.assertTrue("Error result should be instance of IntResult", result is IntResult)
        Assert.assertEquals("Something is going wrong", result.reason)
    }

    @Test
    fun test3OkMethodsReturnValue() {
        val result = IntResult.Ok(value = 10)
        Assert.assertEquals(10, result.getOrDefault(0))
        Assert.assertEquals(10, result.getOrNull())
        Assert.assertEquals(10, result.getStrict())
        Assert.assertEquals(IntResult.Ok(value = 10), result)
        Assert.assertNotEquals(IntResult.Ok(value = 11), result)
    }

    @Test
    fun test4ErrorMethodsNotReturnValue() {
        val result = IntResult.Error(reason = "reason")
        Assert.assertEquals(0, result.getOrDefault(0))
        Assert.assertEquals(null, result.getOrNull())
        val e = assertThrows<NoResultProvided> { result.getStrict() }
        Assert.assertEquals("reason", e.message)
        Assert.assertEquals(IntResult.Error("reason"), result)
        Assert.assertNotEquals(IntResult.Error("bad"), result)
        Assert.assertNotEquals(IntResult.Ok(value = 10), result)
    }

    @Test
    fun test5SafeRunOk() {
        val result = safeRun { 7 + 3 }
        Assert.assertTrue(result is IntResult.Ok)
        Assert.assertEquals(10, result.getStrict())
    }

    @Test
    fun test6SafeRunError() {
        val result = safeRun { error("something went wrong") }
        Assert.assertTrue(result is IntResult.Error)
        Assert.assertEquals("something went wrong", (result as IntResult.Error).reason)
    }

    @Test
    fun test7Magic() {
        val magicCast = "\u0069\u0073\u0053\u0065\u0061\u006c\u0065\u0064"
        val isMagicPresents = IntResult::class.let { wizard ->
            wizard::class.members.find { it.name == magicCast }?.call(wizard) as? Boolean ?: false
        }
        Assert.assertTrue("IntResult interface should be magic (hint think about modifiers)", isMagicPresents)
    }
}
