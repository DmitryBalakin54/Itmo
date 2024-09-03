import time
import types


class TimeoutException(Exception):
    pass


class SoftTimeoutException(TimeoutException):
    pass


class HardTimeoutException(TimeoutException):
    pass


class Time:
    def __init__(self, soft_timeout: float | None = None, hard_timeout: float | None = None) -> None:
        self.soft_timeout = soft_timeout
        self.hard_timeout = hard_timeout
        self.time = 0.0
        self.last_time = 0.0

    def __float__(self) -> float:
        self.last_time = float(time.time() - self.time)
        return self.last_time

    def __str__(self) -> str:
        return 'Time consumed: ' + str(self.__float__())


class TimeCatcher:
    def __init__(self, soft_timeout: float | None = None, hard_timeout: float | None = None) -> None:
        assert soft_timeout is None or soft_timeout > 0
        assert hard_timeout is None or hard_timeout > 0
        if soft_timeout is not None and hard_timeout is not None:
            assert soft_timeout <= hard_timeout

        self.val = Time(soft_timeout, hard_timeout)

    def __enter__(self) -> Time:
        self.val.time = time.time()
        return self.val

    def __exit__(self, exc_type: type, exc_value: BaseException | None, cur_traceback: types.TracebackType) -> bool:
        if exc_value is not None:
            return True

        res = float(self.val)

        soft = self.val.soft_timeout is not None and self.val.soft_timeout < res
        hard = self.val.hard_timeout is not None and self.val.hard_timeout < res

        if soft:
            raise SoftTimeoutException()

        if hard:
            raise HardTimeoutException()

        return False
