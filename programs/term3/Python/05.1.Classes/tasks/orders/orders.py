from dataclasses import dataclass, field, InitVar
from abc import ABC, abstractmethod

DISCOUNT_PERCENTS = 15


@dataclass(init=True, order=True, frozen=True)
class Item:
    # note: you might want to change the order of fields
    item_id: int = field(compare=False)
    title: str
    cost: int

    def __post_init__(self) -> None:
        assert self.cost > 0
        assert len(self.title) > 0


# You may set `# type: ignore` on this class
# It is [a really old issue](https://github.com/python/mypy/issues/5374)
# But seems to be solved
@dataclass(init=True)
class Position(ABC):
    item: Item

    @property
    @abstractmethod
    def cost(self) -> int | float:
        pass


@dataclass(init=True)
class CountedPosition(Position):
    count: int = field(default=1)

    @property
    def cost(self) -> int | float:
        return self.item.cost * self.count


@dataclass(init=True)
class WeightedPosition(Position):
    weight: float = field(default=1.0)

    @property
    def cost(self) -> int | float:
        return self.item.cost * self.weight


@dataclass(init=True, frozen=False)
class Order:
    order_id: int
    positions: list[Position] = field(default_factory=list)
    cost: int = field(default=0)
    have_promo: InitVar[bool] = field(default=False)

    def __post_init__(self,  have_promo: bool) -> None:
        res = sum(i.cost for i in self.positions)
        if have_promo:
            res = res * (100 - DISCOUNT_PERCENTS) / 100

        self.cost = int(res)
