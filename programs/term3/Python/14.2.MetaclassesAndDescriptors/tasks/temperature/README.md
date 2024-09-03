# Дескриптор температуры

`descriptors`

## Условие

Нужно реализовать два дескриптора температуры: `Celsius`, `Kelvin`.

Требования к Kelvin:

1. Использует переменную объекта класса типа `int`
2. На доступ - возвращает значение или кидает AttributeError, если переменной нет
3. Записывает значения только больше 0. При неправильном значении кидает ValueError
4. Нельзя удалить из класса

Требования к Celsius:

1. Использует **поле класса** типа Kelvin
2. Не позовляет записывать значения, кидает AttributeError
3. На доступ:
    1. Проверяет, что работает с полем класса типа Kelvin, если нет - кидает AttributeError
    2. Возвращает значение Kelvin - 273
4. Нельзя удалить из класса

## Пример

```python
class Weather:
    temperature = Kelvin("_temperature")
    celsius = Celsius("temperature")

    def __init__(self, temperature: int):
        self._temperature = 0
        self.temperature = temperature

weather = Weather(10)
print(weather.temperature)  # 10
weather.temperature = 20
print(weather.temperature)  # 20
print(weather.celsius)  # -253

try:
    weather.temperature = -10
except ValueError as e:
    print(e)

try:
    weather.celsius = 0
except AttributeError as e:
    print(e)

class BadWeather:
    temperature = Kelvin("_temperature")

    not_temperature = 0
    celsius = Celsius("not_temperature")

bad = BadWeather()
try:
    print(bad.temperature)
except AttributeError as e:
    print(e)

try:
    bad.temperature = 10
except AttributeError as e:
    print(e)

try:
    print(bad.celsius)
except AttributeError as e:
    print(e)
```
