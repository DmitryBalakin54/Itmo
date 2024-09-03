from __future__ import annotations

import typing as tp

T = tp.TypeVar('T', int, float)


class Pair(tp.Generic[T]):
    def __init__(self, a: T, b: T) -> None:
        self.a: T = a
        self.b: T = b

    def sum(self) -> T:
        return self.a + self.b

    def first(self) -> T:
        return self.a

    def second(self) -> T:
        return self.b

    def __iadd__(self, pair: Pair[T]) -> Pair[T]:
        return Pair(pair.a + self.a, pair.b + self.b)
