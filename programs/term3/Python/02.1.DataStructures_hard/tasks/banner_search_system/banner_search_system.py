import string
from collections import defaultdict
import heapq


def normalize(
        text: str
) -> str:
    """
    Removes punctuation and digits and convert to lower case
    :param text: text to normalize
    :return: normalized query
    """

    return str.lower(text).translate(str.maketrans('', '', string.punctuation) | str.maketrans('', '', string.digits))


def get_words(
        query: str
) -> list[str]:
    """
    Split by words and leave only words with letters greater than 3
    :param query: query to split
    :return: filtered and split query by words
    """

    return [word for word in str.split(query) if len(word) > 3]


def build_index(
        banners: list[str]
) -> dict[str, list[int]]:
    """
    Create index from words to banners ids with preserving order and without repetitions
    :param banners: list of banners for indexation
    :return: mapping from word to banners ids
    """

    dct: defaultdict[str, list[int]] = defaultdict(list[int])

    for ind, s in enumerate(banners):
        for word in set(get_words(normalize(s))):
            dct[word].append(ind)

    return dict(dct)


def merge(seq: list[list[int]]) -> list[int]:

    res: list[int] = []
    k = len(seq)

    if k == 0:
        return res

    iterators = [0 for _ in range(k)]
    heap: list[tuple[int, int]] = []

    for i in range(k):

        if seq[i] is None:
            return res

        if len(seq[i]) > 0:
            heapq.heappush(heap, (seq[i][0], i))
            iterators[i] = 1
        else:
            return res

    min_val = heap[-1][0]
    amount = [x[0] for x in heap].count(min_val)

    while True:
        if amount == k:
            res.append(min_val)

        el, ind = heapq.heappop(heap)

        if el == min_val:
            amount -= 1

        if amount == 0 and len(heap) != 0:
            min_val = heap[-1][0]
            amount = [x[0] for x in heap].count(min_val)

        if len(seq[ind]) == iterators[ind]:
            return res
        else:
            new_el = seq[ind][iterators[ind]]

            heapq.heappush(heap, (new_el, ind))
            iterators[ind] += 1

            if new_el < min_val or len(heap) == 1:
                min_val = new_el
                amount = [x[0] for x in heap].count(min_val)
            elif new_el == min_val:
                amount += 1


def get_banner_indices_by_query(
        query: str,
        index: dict[str, list[int]]
) -> list[int]:
    """
    Extract banners indices from index, if all words from query contains in indexed banner
    :param query: query to find banners
    :param index: index to search banners
    :return: list of indices of suitable banners
    """

    words = get_words(normalize(query))
    lst: list[list[int]] = [index[word] for word in words if index.get(word) is not None]

    if len(lst) != len(words):
        return []

    return merge(lst)


#########################
# Don't change this code
#########################

def get_banners(
        query: str,
        index: dict[str, list[int]],
        banners: list[str]
) -> list[str]:
    """
    Extract banners matched to queries
    :param query: query to match
    :param index: word-banner_ids index
    :param banners: list of banners
    :return: list of matched banners
    """
    indices = get_banner_indices_by_query(query, index)
    return [banners[i] for i in indices]

#########################
