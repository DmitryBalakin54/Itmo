def filter_list_by_list(lst_a: list[int] | range, lst_b: list[int] | range) -> list[int]:
    """
    Filter first sorted list by other sorted list
    :param lst_a: first sorted list
    :param lst_b: second sorted list
    :return: filtered sorted list
    """

    new_lst = []
    i = 0
    j = 0
    while i < len(lst_a) and j < len(lst_b):
        if lst_a[i] < lst_b[j]:
            new_lst.append(lst_a[i])
            i += 1
        elif lst_a[i] > lst_b[j]:
            j += 1
        else:
            i += 1

    while i < len(lst_a):
        new_lst.append(lst_a[i])
        i += 1
    return new_lst
