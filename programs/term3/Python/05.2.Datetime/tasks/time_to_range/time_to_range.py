import datetime
import enum
import typing as tp  # noqa


class GranularityEnum(enum.Enum):
    """
    Enum for describing granularity
    """

    DAY = datetime.timedelta(days=1)
    TWELVE_HOURS = datetime.timedelta(hours=12)
    HOUR = datetime.timedelta(hours=1)
    THIRTY_MIN = datetime.timedelta(minutes=30)
    FIVE_MIN = datetime.timedelta(minutes=5)


def truncate_to_granularity(dt: datetime.datetime, gtd: GranularityEnum) -> datetime.datetime:
    """
    :param dt: datetime to truncate
    :param gtd: granularity
    :return: resulted datetime
    """
    delta = gtd.value
    in_month = datetime.timedelta(days=dt.day, hours=dt.hour, minutes=dt.minute, seconds=dt.second)
    res = in_month // delta * delta
    days = res.days
    hours = res.seconds // (60 * 60)
    minutes = res.seconds // 60 % 60
    seconds = res.seconds % 60
    return datetime.datetime(dt.year, dt.month, days, hours, minutes, seconds)


class DtRange:
    def __init__(
            self,
            before: int,
            after: int,
            shift: int,
            gtd: GranularityEnum
    ) -> None:
        """
        :param before: number of datetimes should take before `given datetime`
        :param after: number of datetimes should take after `given datetime`
        :param shift: shift of `given datetime`
        :param gtd: granularity
        """

        self._before = before
        self._after = after
        self._shift = shift
        self._gtd = gtd

    def __call__(self, dt: datetime.datetime) -> list[datetime.datetime]:
        """
        :param dt: given datetime
        :return: list of datetimes in range
        """

        date = truncate_to_granularity(dt, self._gtd) + self._shift * self._gtd.value
        res = []
        for i in range(self._before + self._after + 1):
            res.append(date + (i - self._before) * self._gtd.value)
        return res


def get_interval(
        start_time: datetime.datetime,
        end_time: datetime.datetime,
        gtd: GranularityEnum
) -> list[datetime.datetime]:
    """
    :param start_time: start of interval
    :param end_time: end of interval
    :param gtd: granularity
    :return: list of datetimes according to granularity
    """

    start = truncate_to_granularity(start_time, gtd)
    res = []
    while start <= end_time:
        if start_time <= start:
            res.append(start)
        start += gtd.value

    return res
