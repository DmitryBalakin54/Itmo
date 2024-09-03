import inspect
import pytest

from .animals import Cat, Cow, Dog
from .animals_factory import Animal, CatAdapter, CowAdapter, DogAdapter, animals_factory


def test_cat() -> None:
    processed = animals_factory(Cat())
    assert isinstance(processed, CatAdapter)
    assert processed.say() == "meow"


def test_cow() -> None:
    processed = animals_factory(Cow())
    assert isinstance(processed, CowAdapter)
    assert processed.say() == "moo"


def test_dog() -> None:
    processed = animals_factory(Dog())
    assert isinstance(processed, DogAdapter)
    assert processed.say() == "woof"


def test_animal_abstract() -> None:
    assert inspect.isabstract(Animal)


def test_type_error_on_wrong_type() -> None:
    with pytest.raises(TypeError):
        animals_factory(1)
