from collections.abc import Callable
from typing import Any, TypeVar
from collections import OrderedDict
import functools
import typing as tp

Function = TypeVar('Function', bound=Callable[..., Any])


def cache(max_size: int) -> Callable[[Function], Function]:
    """
    Returns decorator, which stores result of function
    for `max_size` most recent function arguments.
    :param max_size: max amount of unique arguments to store values for
    :return: decorator, which wraps any function passed
    """

    cache_dct: OrderedDict[tuple[tuple[Any, ...], tuple[Any, ...]], Any] = OrderedDict()

    def decorate(func: Function) -> Function:
        @functools.wraps(func)
        def wrapper(*args: Any, **kwargs: Any) -> Any:
            if cache_dct.get((tuple(args), tuple(kwargs))) is not None:
                return cache_dct[(tuple(args), tuple(kwargs))]
            else:
                if len(cache_dct) == max_size:
                    cache_dct.popitem(last=False)

                res = func(*args, **kwargs)
                cache_dct[(tuple(args), tuple(kwargs))] = res
                return res

        return tp.cast(Function, wrapper)

    return decorate
