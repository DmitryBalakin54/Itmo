def reverse_iterative(lst: list[int]) -> list[int]:
    """
    Return reversed list. You can use only iteration
    :param lst: input list
    :return: reversed list
    """

    new_lst = []
    for i in range(len(lst)):
        new_lst.append(lst[len(lst) - i - 1])

    return new_lst


def reverse_inplace_iterative(lst: list[int]) -> None:
    """
    Revert list inplace. You can use only iteration
    :param lst: input list
    :return: None
    """

    for i in range(len(lst) // 2):
        tmp = lst[i]
        lst[i] = lst[-i - 1]
        lst[-i - 1] = tmp


def reverse_inplace(lst: list[int]) -> None:
    """
    Revert list inplace with reverse method
    :param lst: input list
    :return: None
    """

    lst.reverse()


def reverse_reversed(lst: list[int]) -> list[int]:
    """
    Revert list with `reversed`
    :param lst: input list
    :return: reversed list
    """

    return list(reversed(lst))


def reverse_slice(lst: list[int]) -> list[int]:
    """
    Revert list with slicing
    :param lst: input list
    :return: reversed list
    """

    return lst[::-1]
