from collections.abc import Iterable, Iterator, Sized


class RangeIterator(Iterator[int]):
    """The iterator class for Range"""
    def __init__(self, range_: 'Range') -> None:
        self.el = range_.start
        self.stop = range_.stop
        self.step = range_.step

    def __iter__(self) -> 'RangeIterator':
        return self

    def __next__(self) -> int:
        res = self.el
        self.el += self.step
        if self.step > 0:
            if res < self.stop:
                return res
            else:
                raise StopIteration
        elif self.stop < res:
            return res
        else:
            raise StopIteration


class Range(Sized, Iterable[int]):
    """The range-like type, which represents an immutable sequence of numbers"""

    def __init__(self, *args: int) -> None:
        """
        :param args: either it's a single `stop` argument
            or sequence of `start, stop[, step]` arguments.
        If the `step` argument is omitted, it defaults to 1.
        If the `start` argument is omitted, it defaults to 0.
        If `step` is zero, ValueError is raised.
        """
        if len(args) == 1:
            self.stop = args[0]
            self.start = 0
            self.step = 1
            self.str = f'range({self.start}, {self.stop})'
        elif len(args) == 2:
            self.start, self.stop = args[0], args[1]
            self.step = 1
            self.str = f'range({self.start}, {self.stop})'
        elif len(args) == 3:
            self.start, self.stop, self.step = args[0], args[1], args[2]
            if self.step == 1:
                self.str = f'range({self.start}, {self.stop})'
            else:
                self.str = f'range({self.start}, {self.stop}, {self.step})'
            assert self.step != 0

    def __iter__(self) -> 'RangeIterator':
        return RangeIterator(self)

    def __repr__(self) -> str:
        return self.str

    def __str__(self) -> str:
        return self.str

    def __contains__(self, key: int) -> bool:
        min_v = min(self.start, self.stop)
        max_v = self.start + self.stop - min_v
        step = self.step
        if min_v == self.start and step > 0:
            return key - min_v >= 0 and (key - min_v) % step == 0 and key < self.stop
        elif max_v == self.start and step < 0:
            return max_v - key >= 0 and (key - max_v) % step == 0 and self.stop < key
        else:
            return False

    def __getitem__(self, key: int) -> int:
        res = self.start + key * self.step
        if self.__contains__(res):
            return res
        else:
            raise IndexError

    def __len__(self) -> int:
        if self.__contains__(self.start):
            res = abs((self.start - self.stop) // self.step)
            return res if res > 0 else 1
        else:
            return 0
