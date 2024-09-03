# Property converter

`metaclass` `property`

## Условие

Есть старые классы, в которых вместо property используются setter и getter функции.

```python
class OldAndNasty:
    def __init__(self, temperature):
        self._temperature = temperature

    def get_temperature(self):
        return self._temperature

    def set_temperature(self, temperature):
        if temperature <= 0:
            raise ValueError("Temperature below zero is not allowed")
        self._temperature = temperature
```

В новом коде хочется использовать старые классы, но с интерфейсом property.
Ваша задача - реализовать метакласс `PropertyConverter`, который даст доступ к тем же свойствам через property.
Название property должно получаться откидыванием приставок `get_` и `set_` от старых функций.
Логика property должна быть идентична логике `get_` и `set_`.

То есть вот такой класс:

```python
class NewAndShiny(OldAndNasty, PropertyConverter):
    pass
```

Должен быть эквивалентен такому:

```python
class NewAndShiny(OldAndNasty):
    @property
    def temperature(self):
        return self.get_temperature()

    @temperature.setter
    def temperature(self, temperature)
        self.set_temperature(temperature)
```

## Пример

```python
class NewAndShiny(OldAndNasty, PropertyConverter):
    pass

# старые функции работают
new_obj = NewAndShiny(100)
new_obj.set_temperature(10)
try:
    new_obj.set_temperature(-1)
except ValueError:
    print("Did not set")

# но есть property
print(new_obj.get_temperature())
new_obj.temperature = 50
print(new_obj.temperature)
try:
    new_obj.temperature = -1
except ValueError:
    print("Did not set")
```

## Замечания

* Чтобы предотвратить ошибки typing можно добавить в PropertyConverter `__getattr__`, `__setattr__` 
* Нужно сделать реализацию через метаклассы, другие решения могут не проходить тесты
* Не пытайтесь переопределять конкретные методы класса из публичного теста, решение должно быть общим
