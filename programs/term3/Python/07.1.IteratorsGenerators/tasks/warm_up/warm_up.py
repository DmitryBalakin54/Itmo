from collections.abc import Generator
from typing import Any


def transpose(matrix: list[list[Any]]) -> list[list[Any]]:
    """
    :param matrix: rectangular matrix
    :return: transposed matrix
    """

    return [[*i] for i in zip(*matrix)]


def uniq(sequence: list[Any]) -> Generator[Any, None, None]:
    """
    :param sequence: arbitrary sequence of comparable elements
    :return: generator of elements of `sequence` in
    the same order without duplicates
    """
    st = set()
    for i in sequence:
        if i not in st:
            yield i
        st.add(i)


def dict_merge(*dicts: dict[Any, Any]) -> dict[Any, Any]:
    """
    :param *dicts: flat dictionaries to be merged
    :return: merged dictionary
    """
    return {key: val for dct in dicts for key, val in dct.items()}


def product(lhs: list[int], rhs: list[int]) -> int:
    """
    :param rhs: first factor
    :param lhs: second factor
    :return: scalar product
    """

    return sum(i * j for i, j in zip(lhs, rhs))
