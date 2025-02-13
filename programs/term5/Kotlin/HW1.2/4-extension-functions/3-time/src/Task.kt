val Int.milliseconds: Time
    get() = correctTime(0, this)

val Int.seconds: Time
    get() = this.milliseconds * 1000

val Int.minutes: Time
    get() = this.seconds * 60

val Int.hours: Time
    get() = this.minutes * 60

operator fun Time.plus(other: Time): Time = correctTime(
    this.seconds + other.seconds,
    this.milliseconds + other.milliseconds,
)

operator fun Time.minus(other: Time): Time = minusImpl(
    this.seconds - other.seconds,
    this.milliseconds - other.milliseconds,
)

operator fun Time.times(times: Int): Time = correctTime(
    this.seconds * times,
    this.milliseconds * times,
)

fun minusImpl(seconds: Long, milliseconds: Int): Time {
    var newSeconds = seconds
    var newMilliseconds = milliseconds
    if (milliseconds < 0) {
        newSeconds -= 1
        newMilliseconds += 1000
    }

    return correctTime(newSeconds, newMilliseconds)
}

fun correctTime(seconds: Long, milliseconds: Int): Time {
    return Time(seconds + milliseconds / 1000, milliseconds % 1000)
}
