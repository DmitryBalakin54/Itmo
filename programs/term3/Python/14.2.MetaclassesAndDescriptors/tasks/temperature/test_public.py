import pytest

from .temperature import Celsius, Kelvin


class Weather:
    temperature = Kelvin("_temperature")
    celsius = Celsius("temperature")

    def __init__(self, temperature: int):
        self._temperature = 0
        self.temperature = temperature


def test_constructor_raises() -> None:
    with pytest.raises(ValueError):
        Weather(-20)


def test_kelvin_get_ok() -> None:
    weather = Weather(10)
    assert weather.temperature == 10


def test_kelvin_set_ok() -> None:
    weather = Weather(10)
    weather.temperature = 30
    assert weather.temperature == 30


def test_kelvin_set_raises() -> None:
    weather = Weather(10)
    with pytest.raises(ValueError):
        weather.temperature = -300


def test_kelvin_descriptor_different() -> None:
    weather1 = Weather(10)
    weather2 = Weather(50)
    assert weather1.temperature == 10
    assert weather2.temperature == 50


def test_kelvin_del_raises() -> None:
    weather = Weather(10)
    with pytest.raises(ValueError):
        del weather.temperature


def test_celsius_get_ok() -> None:
    weather = Weather(273)
    assert weather.celsius == 0


def test_celsius_set_raises() -> None:
    weather = Weather(273)
    with pytest.raises(AttributeError):
        weather.celsius = 10


def test_celsius_del_raises() -> None:
    weather = Weather(10)
    with pytest.raises(ValueError):
        del weather.celsius


class BadKelvin:
    temperature = Kelvin("_temperature")


def test_raise_attribute_error_on_bad_kelvin() -> None:
    bad = BadKelvin()
    with pytest.raises(AttributeError):
        print(bad.temperature)
    with pytest.raises(AttributeError):
        bad.temperature = 10


class BadCelsius:
    temperature = 10
    celsius = Celsius("temperature")


def test_raise_attribute_error_on_bad_celsius() -> None:
    bad = BadCelsius()
    with pytest.raises(AttributeError):
        print(bad.celsius)
