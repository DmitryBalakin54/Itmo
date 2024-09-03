import datetime
import functools
import inspect


def profiler(func):  # type: ignore
    """
    Returns profiling decorator, which counts calls of function
    and measure last function execution time.
    Results are stored as function attributes: `calls`, `last_time_taken`
    :param func: function to decorate
    :return: decorator, which wraps any function passed
    """

    func.calls = 1
    func.last_time_taken = datetime.datetime.now()

    @functools.wraps(func)
    def wrapper(*argc, **kwargs):   # type: ignore
        if inspect.stack()[1][3] == func.__name__:
            func.calls += 1
        else:
            func.calls = 1
            func.last_time_taken = datetime.datetime.now()

        res = func(*argc, **kwargs)

        if inspect.stack()[1][3] != func.__name__:
            func.last_time_taken = datetime.datetime.now() - func.last_time_taken
            func.last_time_taken = func.last_time_taken.total_seconds()
        return res
    wrapper.__dict__ = func.__dict__

    return wrapper
