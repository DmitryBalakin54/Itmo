import typing as tp
from abc import ABC, abstractmethod
from .animals import Cat, Cow, Dog


class Animal(ABC):
    @abstractmethod
    def say(self) -> str:
        pass


class CatAdapter(Animal):
    def __init__(self, cat: Cat):
        self.cat = cat

    def say(self) -> str:
        return self.cat.say()


class DogAdapter(Animal):
    def __init__(self, dog: Dog):
        self.dog = dog

    def say(self) -> str:
        return self.dog.say("woof")


class CowAdapter(Animal):
    def __init__(self, cow: Cow):
        self.cow = cow

    def say(self) -> str:
        return self.cow.talk()


def animals_factory(animal: tp.Any) -> Animal:
    if isinstance(animal, Dog):
        return DogAdapter(animal)
    elif isinstance(animal, Cow):
        return CowAdapter(animal)
    elif isinstance(animal, Cat):
        return CatAdapter(animal)
    else:
        raise TypeError("Unsupported animal type")
