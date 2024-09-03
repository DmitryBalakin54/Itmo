import multiprocessing
import threading
import time


def very_slow_function(x: int) -> int:
    """Function which calculates square of given number really slowly
    :param x: given number
    :return: number ** 2
    """
    time.sleep(0.3)
    return x ** 2


def calc_squares_simple(bound: int) -> list[int]:
    """Function that calculates squares of numbers in range [0; bound)
    :param bound: positive upper bound for range
    :return: list of squared numbers
    """

    return [very_slow_function(i) for i in range(bound)]


def calc_squares_multithreading(bound: int) -> list[int]:
    """Function that calculates squares of numbers in range [0; bound)
    using threading.Thread
    :param bound: positive upper bound for range
    :return: list of squared numbers
    """
    def f(x: int, result: list[int], ind: int) -> None:
        result[ind] = very_slow_function(x)

    res = [0] * bound
    threads = [threading.Thread(target=f, args=(i, res, i)) for i in range(bound)]
    for thread in threads:
        thread.start()

    for thread in threads:
        thread.join()

    return res


def calc_squares_multiprocessing(bound: int) -> list[int]:
    """Function that calculates squares of numbers in range [0; bound)
    using multiprocessing.Pool
    :param bound: positive upper bound for range
    :return: list of squared numbers
    """

    with multiprocessing.Pool() as pool:
        res = pool.map(very_slow_function, range(bound))

    return res
