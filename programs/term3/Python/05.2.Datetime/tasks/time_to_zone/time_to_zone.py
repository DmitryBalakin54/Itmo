import zoneinfo
from datetime import datetime

DEFAULT_TZ_NAME = "Europe/Moscow"
ZONE_INFO = zoneinfo.ZoneInfo(DEFAULT_TZ_NAME)


def now() -> datetime:
    """Return now in default timezone"""
    return datetime.now().astimezone(tz=ZONE_INFO)


def strftime(dt: datetime, fmt: str) -> str:
    """Return dt converted to string according to format in default timezone"""

    new_dt = dt.astimezone(tz=ZONE_INFO) if dt.tzinfo is not None else dt
    return new_dt.replace(tzinfo=ZONE_INFO).strftime(fmt)


def strptime(dt_str: str, fmt: str) -> datetime:
    """Return dt parsed from string according to format in default timezone"""

    dt = datetime.strptime(dt_str, fmt)
    new_dt = dt.astimezone(tz=ZONE_INFO) if dt.tzinfo is not None else dt
    return new_dt.replace(tzinfo=ZONE_INFO)


def diff(first_dt: datetime, second_dt: datetime) -> int:
    """Return seconds between two datetimes rounded down to closest int"""

    f = first_dt.astimezone(tz=ZONE_INFO) if first_dt.tzinfo is not None else first_dt.replace(tzinfo=ZONE_INFO)
    s = second_dt.astimezone(tz=ZONE_INFO) if second_dt.tzinfo is not None else second_dt.replace(tzinfo=ZONE_INFO)
    res = s - f
    return int(res.seconds + res.days * 24 * 60 * 60)


def timestamp(dt: datetime) -> int:
    """Return timestamp for given datetime rounded down to closest int"""

    new_dt = dt.astimezone(tz=ZONE_INFO) if dt.tzinfo is not None else dt.replace(tzinfo=ZONE_INFO)
    return int(new_dt.timestamp())


def from_timestamp(ts: float) -> datetime:
    """Return datetime from given timestamp"""

    return datetime.fromtimestamp(ts).astimezone(tz=ZONE_INFO)
