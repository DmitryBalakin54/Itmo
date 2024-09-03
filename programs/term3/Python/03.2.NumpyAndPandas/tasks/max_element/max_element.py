import numpy as np
import numpy.typing as npt


def max_element(array: npt.NDArray[np.int_]) -> int | None:
    """
    Return max element before zero for input array.
    If appropriate elements are absent, then return None
    :param array: array,
    :return: max element value or None
    """
    if 0 not in array:
        return None

    zeros = np.where(array == 0)[0]

    if zeros[-1] == len(array) - 1:
        zeros = zeros[:-1]

    if len(zeros) == 0:
        return None

    max_num = np.max(array[zeros + 1])

    return int(max_num)
