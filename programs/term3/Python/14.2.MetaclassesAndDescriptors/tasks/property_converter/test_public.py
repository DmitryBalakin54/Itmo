import pytest
import typing as tp

from .property_converter import PropertyConverter


class OldAndNastyOnlyGetter:
    def __init__(self, temperature: float) -> None:
        self._temperature = temperature

    def get_temperature(self) -> float:
        return self._temperature


def test_property_get() -> None:
    class NewAndShiny(OldAndNastyOnlyGetter, PropertyConverter):
        pass

    n = NewAndShiny(100)
    assert n.get_temperature() == 100
    assert n.temperature == 100
    with pytest.raises(AttributeError):
        n.temperature = 10


class OldAndNasty:
    def __init__(self, temperature: float) -> None:
        self._temperature = temperature
        self.x = 0

    def get_temperature(self) -> float:
        return self._temperature

    def set_temperature(self, temperature: float) -> None:
        if temperature <= 0:
            raise ValueError("Temperature below zero is not allowed")
        self._temperature = temperature

    def __getattribute__(self, attr: str) -> tp.Any:
        return object.__getattribute__(self, attr)

    def __getattr__(self, attr: str) -> tp.Any:
        return object.__getattribute__(self, attr)

    def __setattr__(self, attr: str, value: tp.Any) -> None:
        object.__setattr__(self, attr, value)


def test_property_set() -> None:
    class NewAndShiny(OldAndNasty, PropertyConverter):
        pass

    n = NewAndShiny(100)
    n.temperature = 10
    assert n.temperature == 10
    with pytest.raises(ValueError):
        n.temperature = -1


def test_override_property() -> None:
    class NewAndShiny(OldAndNasty, PropertyConverter):
        @property
        def temperature(self) -> float:
            return self._temperature * 2

        @temperature.setter
        def temperature(self, temperature: float) -> None:
            self._temperature = temperature * 2

    n = NewAndShiny(100)
    assert n.temperature == 200
    n.temperature = 100
    assert n.temperature == 400


def test_not_get_set_attr() -> None:
    class NewAndShiny(OldAndNasty, PropertyConverter):
        def __getattr__(self, attr: str) -> None:
            raise AttributeError()

        def __setattr__(self, attr: str, value: tp.Any) -> None:
            object.__setattr__(self, attr, value)
    n = NewAndShiny(100)
    assert n.temperature == 100
    n.temperature = 10
    assert n.temperature == 10


class OldAndNastyPrice:
    def __init__(self, mean_price: float) -> None:
        self._mean_price = mean_price
        self.x = 0

    def get_mean_price(self) -> float:
        return self._mean_price

    def set_mean_price(self, mean_price: float) -> None:
        if mean_price <= 0:
            raise ValueError("Mean price below zero is not allowed")
        self._mean_price = mean_price


def test_property_with_underscore_set() -> None:
    class NewAndShinyPrice(OldAndNastyPrice, PropertyConverter):
        pass

    n = NewAndShinyPrice(100)
    n.mean_price = 10
    assert n.mean_price == 10
    with pytest.raises(ValueError):
        n.mean_price = -1
