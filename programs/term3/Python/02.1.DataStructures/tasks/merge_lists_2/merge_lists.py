import typing as tp
import heapq


def merge(seq: tp.Sequence[tp.Sequence[int]]) -> list[int]:
    """
    :param seq: sequence of sorted sequences
    :return: merged sorted list
    """

    k = len(seq)
    iterators = [0 for _ in range(k)]
    heap: list[tuple[int, int]] = []
    res = []

    for i in range(k):
        if len(seq[i]) > 0:
            heapq.heappush(heap, (seq[i][0], i))
            iterators[i] = 1
        else:
            k -= 1

    while k > 0:
        el, ind = heapq.heappop(heap)
        res.append(el)

        if len(seq[ind]) == iterators[ind]:
            k -= 1
        else:
            heapq.heappush(heap, (seq[ind][iterators[ind]], ind))
            iterators[ind] += 1

    return res
