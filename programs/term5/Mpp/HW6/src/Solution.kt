/**
 * @author : Balakin Dmitry
 */
class Solution : MonotonicClock {
    private var wl1 by RegularInt(0)
    private var wl2 by RegularInt(0)
    private var wl3 by RegularInt(0)
    private var wr1 by RegularInt(0)
    private var wr2 by RegularInt(0)

    override fun write(time: Time) {
        wr1 = time.d1
        wr2 = time.d2

        wl3 = time.d3
        wl2 = time.d2
        wl1 = time.d1
    }

    override fun read(): Time {
        val rl1 = wl1
        val rl2 = wl2
        val rl3 = wl3

        val rr2 = wr2
        val rr1 = wr1

        if (rr1 != rl1) {
            return Time(rr1, 0, 0)
        }

        if (rr2 != rl2) {
            return Time(rr1, rr2, 0)
        }

        return Time(rl1, rl2, rl3)
    }
}