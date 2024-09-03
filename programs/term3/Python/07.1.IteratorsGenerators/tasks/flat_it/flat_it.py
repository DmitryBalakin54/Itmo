from collections.abc import Iterable, Iterator
from typing import Any


def flat_it(sequence: Iterable[Any]) -> Iterator[Any]:
    """
    :param sequence: iterable with arbitrary level of nested iterables
    :return: generator producing flatten sequence
    """

    for el in sequence:
        if type(el) is str and len(el) <= 1:
            yield el
            continue

        try:
            iter(el)
            yield from flat_it(el)
        except TypeError:
            yield el
