import numpy as np
import numpy.typing as npt


def nearest_value(matrix: npt.NDArray[np.float_], value: float) -> float | None:
    """
    Find nearest value in matrix.
    If matrix is empty return None
    :param matrix: input matrix
    :param value: value to find
    :return: nearest value in matrix or None
    """
    line = np.ravel(matrix)
    if len(line) == 0:
        return None
    vec = np.zeros(line.shape)
    vec.fill(value)
    offset_line = np.absolute(line - vec)
    min_offset = np.min(offset_line)
    return float(line[np.where(offset_line == min_offset)[0][0]])
