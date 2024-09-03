import typing as tp
from collections import defaultdict


def revert(dct: tp.Mapping[str, str]) -> dict[str, list[str]]:
    """
    :param dct: dictionary to revert in format {key: value}
    :return: reverted dictionary {value: [key1, key2, key3]}
    """

    rev_dct: defaultdict[str, list[str]] = defaultdict(list)

    for key, val in dct.items():
        rev_dct[val].append(key)

    return dict(rev_dct)
