# Фабрика животных

`patterns`

## Условие

Есть 3 животных:

```python
# animals/animals.py

class Cat:
    def say(self) -> str:
        return "meow"


class Dog:
    def say(self, what: str) -> str:
        return what


class Cow:
    def talk(self) -> str:
        return "moo"
```

1. Адаптировать всех животных к одному интерфейсу Animal с методом say()
2. Интерфейс должен быть настоящим интерфейсом - нельзя создать его объект.
3. Реализовать фабрику, которая конвертирует животное в интерфейс
4. Фабрика должна бросать TypeError на некорректный тип.

## Пример

```python
dog = animals_factory(Dog())
print(dog.say())  # woof
```
