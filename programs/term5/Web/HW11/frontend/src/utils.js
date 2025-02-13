export function dateFormat (dateStr) {
    const [date, time] = dateStr.split("T");

    const [correctTime, _] = time.split(".")

    const [year, month, day] = date.split("-")

    return `${day}.${month}.${year} ${correctTime}`
}