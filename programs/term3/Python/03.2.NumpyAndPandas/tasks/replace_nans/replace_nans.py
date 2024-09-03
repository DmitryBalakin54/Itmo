import numpy as np
import numpy.typing as npt


def replace_nans(matrix: npt.NDArray[np.float_]) -> npt.NDArray[np.float_]:
    """
    Replace all nans in matrix with average of other values.
    If all values are nans, then return zero matrix of the same size.
    :param matrix: matrix,
    :return: replaced matrix
    """
    new_matrix = matrix.copy()

    if np.all(np.isnan(matrix)):
        val = 0
    else:
        val = new_matrix[~np.isnan(matrix)].mean()

    new_matrix[np.isnan(matrix)] = val
    return new_matrix
