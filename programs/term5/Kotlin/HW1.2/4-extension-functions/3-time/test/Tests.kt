import org.junit.Assert
import org.junit.FixMethodOrder
import org.junit.Test
import org.junit.runners.MethodSorters

@FixMethodOrder(MethodSorters.NAME_ASCENDING)
class Tests {
    @Test
    fun testExtensionMilliseconds() {
        Assert.assertEquals("0.milliseconds should be 0.000", Time(0, 0), 0.milliseconds)
        Assert.assertEquals("777.milliseconds should be 0.777", Time(0, 777), 777.milliseconds)
        Assert.assertEquals("13212.milliseconds should be 13.212", Time(13, 212), 13212.milliseconds)
    }

    @Test
    fun testExtensionSeconds() {
        Assert.assertEquals("0.seconds should be 0.0", Time(0, 0), 0.seconds)
        Assert.assertEquals("42.seconds should be 42.0", Time(42, 0), 42.seconds)
    }

    @Test
    fun testExtensionMinutes() {
        Assert.assertEquals("0.minutes should be 0.0", Time(0, 0), 0.minutes)
        Assert.assertEquals("12.minutes should be 720.0", Time(720, 0), 12.minutes)
    }

    @Test
    fun testExtensionHours() {
        Assert.assertEquals("0.hours should be 0.0", Time(0, 0), 0.hours)
        Assert.assertEquals("3.hours should be 10800.0", Time(10800, 0), 3.hours)
    }

    @Test
    fun testOperationsPlus() {
        Assert.assertEquals("0.0 + 10.012 should be 10.012", Time(10, 12), Time(0, 0) + Time(10, 12))
        Assert.assertEquals("11.0 + 0.999 should be 11.999", Time(11, 999), Time(11, 0) + Time(0, 999))
        Assert.assertEquals("1.999 + 3.001 should be 5.000", Time(5, 0), Time(1, 999) + Time(3, 1))
        Assert.assertEquals("10.900 + 12.800 should be 23.700", Time(23, 700), Time(10, 900) + Time(12, 800))
    }

    @Test

    fun testOperationsSubtraction() {
        Assert.assertEquals("10.222 - 10.222 should be 0.000", Time(0, 0), Time(10, 222) - Time(10, 222))
        Assert.assertEquals("33.999 - 32.998 should be 1.001", Time(1, 1), Time(33, 999) - Time(32, 998))
        Assert.assertEquals("12.001 - 0.000 should be 12.001", Time(12, 1), Time(12, 1) - Time(0, 0))

        assertThrows<IllegalArgumentException>("10.900 - 12.800 should thrown IllegalArgumentException") {
            Time(10, 900) - Time(12, 800)
        }
        assertThrows<IllegalArgumentException>("10.000 - 12.800 should thrown IllegalArgumentException") {
            Time(10, 0) - Time(12, 800)
        }
    }

    @Test
    fun testOperationsMultiplication() {
        Assert.assertEquals("0.000 * 2 should be 0.000", Time(0, 0), Time(0, 0) * 2)
        Assert.assertEquals("1.123 * 2 should be 3.369", Time(3, 369), Time(1, 123) * 3)
        Assert.assertEquals("4.321 * 7 should be 30.247", Time(30, 247), Time(4, 321) * 7)
    }
}
