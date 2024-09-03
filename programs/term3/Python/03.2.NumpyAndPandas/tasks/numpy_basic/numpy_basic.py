import numpy as np
import numpy.typing as npt


def construct_array(
        matrix: npt.NDArray[np.int_],
        row_indices: npt.NDArray[np.int_] | list[int],
        col_indices: npt.NDArray[np.int_] | list[int]
) -> npt.NDArray[np.int_]:
    """
    Construct slice of given matrix by indices row_indices and col_indices:
    [matrix[row_indices[0], col_indices[0]], ... , matrix[row_indices[N-1], col_indices[N-1]]]
    :param matrix: input matrix
    :param row_indices: list of row indices
    :param col_indices: list of column indices
    :return: matrix slice
    """

    return matrix[row_indices, col_indices]


def detect_identic(
        lhs_array: npt.ArrayLike,
        rhs_array: npt.ArrayLike
) -> bool:
    """
    Check whether two arrays are equal or not
    :param lhs_array: first array
    :param rhs_array: second array
    :return: True if input arrays are equal, False otherwise
    """

    return np.array_equal(rhs_array, lhs_array)


def mean_channel(X: npt.NDArray[np.float_]) -> npt.NDArray[np.float_]:
    """
    Given color image (3-dimensional vector of size (n, m, 3).
    Compute average value for all 3 channels
    :param X: color image
    :return: array of size 3 with average values
    """
    if len(np.ravel(X)) == 0:
        return np.array([np.nan] * 3)
    return np.mean(X, axis=(0, 1))


def get_unique_rows(X: npt.NDArray[np.int_]) -> npt.NDArray[np.int_]:
    """
    Compute unique rows of matrix
    :param X: matrix
    :return: matrix of unique rows
    """

    matrix = np.sort(X, axis=0)
    return np.unique(matrix, axis=0)


def construct_matrix(
        first_array: npt.NDArray[np.int_], second_array: npt.NDArray[np.int_]
) -> npt.NDArray[np.int_]:
    """
    Construct matrix from pair of arrays
    :param first_array: first array
    :param second_array: second array
    :return: constructed matrix
    """

    return np.concatenate((first_array.reshape(-1, 1), second_array.reshape(-1, 1)), axis=1)
