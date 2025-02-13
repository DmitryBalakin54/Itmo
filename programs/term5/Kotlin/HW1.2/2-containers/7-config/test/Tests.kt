import io.mockk.every
import io.mockk.mockkStatic
import io.mockk.verify
import org.junit.Assert
import org.junit.Before
import org.junit.Test

class Tests {
    @Test
    fun testCreateConfigBasedOnValidFile() {
        Config(configName)
        verify { getResource(eq(configName)) }
    }

    @Test
    fun testCreateConfigBasedOnValidInvalidFileName() {
        assertThrows<IllegalArgumentException> { Config("undefined") }
        verify { getResource(eq("undefined")) }
    }

    @Test
    @Suppress("LocalVariableName")
    fun testConfigInstance() {
        val config = Config(configName)

        val value by config
        val other by config
        val `name with spaces` by config
        val name_with_underscore by config

        Assert.assertEquals("10", value)
        Assert.assertEquals("stringValue", other)
        Assert.assertEquals("value_with_underscore", `name with spaces`)
        Assert.assertEquals("value with spaces", name_with_underscore)

        assertThrows<IllegalArgumentException> { val invalid by config }
    }

    @Before
    fun before() {
        mockkStatic(::getResource)
        every { getResource(eq(configName)) } answers { configContent.byteInputStream() }
        every { getResource(neq(configName)) } returns null
    }

    private val configName = "config"
    private val configContent =
        """
            |value = 10
            |other = stringValue
            | name with spaces   = value_with_underscore
            |name_with_underscore =   value with spaces  
            """.trimMargin()
}
