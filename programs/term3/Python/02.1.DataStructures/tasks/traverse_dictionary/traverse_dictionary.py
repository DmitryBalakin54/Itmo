import typing as tp


def traverse_dictionary_immutable(
        dct: tp.Mapping[str, tp.Any],
        prefix: str = "") -> list[tuple[str, int]]:
    """
    :param dct: dictionary of undefined depth with integers or other dicts as leaves with same properties
    :param prefix: prefix for key used for passing total path through recursion
    :return: list with pairs: (full key from root to leaf joined by ".", value)
    """

    res: list[tuple[str, int]] = []

    for key, val in dct.items():
        if type(val) is int:
            res.append((prefix + key, val))
        else:
            res.extend(traverse_dictionary_immutable(val, prefix + key + '.'))

    return res


def traverse_dictionary_mutable(
        dct: tp.Mapping[str, tp.Any],
        result: list[tuple[str, int]],
        prefix: str = "") -> None:
    """
    :param dct: dictionary of undefined depth with integers or other dicts as leaves with same properties
    :param result: list with pairs: (full key from root to leaf joined by ".", value)
    :param prefix: prefix for key used for passing total path through recursion
    :return: None
    """

    for key, val in dct.items():
        if type(val) is int:
            result.append((prefix + key, val))
        else:
            traverse_dictionary_mutable(val, result, prefix + key + '.')


def traverse_dictionary_iterative(
        dct: tp.Mapping[str, tp.Any]
) -> list[tuple[str, int]]:
    """
    :param dct: dictionary of undefined depth with integers or other dicts as leaves with same properties
    :return: list with pairs: (full key from root to leaf joined by ".", value)
    """

    sub_dct: dict[str, tp.Any] = dict()
    for key, val in dct.items():
        sub_dct[key] = val

    res: list[tuple[str, int]] = []

    while len(sub_dct) > 0:
        key, val = sub_dct.popitem()
        if type(val) is int:
            res.append((key, val))
        else:
            for k, v in dict(val).items():
                sub_dct[key + '.' + k] = v

    return res
