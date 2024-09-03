from types import FunctionType
from typing import Any

CO_VARARGS = 4
CO_VARKEYWORDS = 8

ERR_TOO_MANY_POS_ARGS = 'Too many positional arguments'
ERR_TOO_MANY_KW_ARGS = 'Too many keyword arguments'
ERR_MULT_VALUES_FOR_ARG = 'Multiple values for arguments'
ERR_MISSING_POS_ARGS = 'Missing positional arguments'
ERR_MISSING_KWONLY_ARGS = 'Missing keyword-only arguments'
ERR_POSONLY_PASSED_AS_KW = 'Positional-only argument passed as keyword argument'


def bind_args(func: FunctionType, *args: Any, **kwargs: Any) -> dict[str, Any]:
    """Bind values from `args` and `kwargs` to corresponding arguments of `func`

    :param func: function to be inspected
    :param args: positional arguments to be bound
    :param kwargs: keyword arguments to be bound
    :return: `dict[argument_name] = argument_value` if binding was successful,
             raise TypeError with one of `ERR_*` error descriptions otherwise
    """

    have_args = bool(CO_VARARGS & func.__code__.co_flags)
    have_kwargs = bool(CO_VARKEYWORDS & func.__code__.co_flags)

    argc = func.__code__.co_argcount
    names_count = argc + func.__code__.co_kwonlyargcount + have_args + have_kwargs
    arg_names = [*func.__code__.co_varnames][:names_count]

    res: dict[str, Any] = {}

    if names_count == 0:
        if not (len(args) == 0 or have_args):
            raise TypeError(ERR_TOO_MANY_POS_ARGS)
        return res

    if argc < len(args) and not have_args:
        raise TypeError(ERR_TOO_MANY_POS_ARGS)

    defaults = [*func.__defaults__] if func.__defaults__ else []
    kw_defaults = func.__kwdefaults__ if func.__kwdefaults__ else {}
    pos = func.__code__.co_posonlyargcount

    args_name = ""
    kwargs_name = ""

    if have_args:
        if have_kwargs:
            kwargs_name = arg_names[-1]
            args_name = arg_names[-2]
            res[args_name] = []
            res[kwargs_name] = {}
        else:
            args_name = arg_names[-1]
            res[args_name] = []
    elif have_kwargs:
        kwargs_name = arg_names[-1]
        res[kwargs_name] = {}

    for j in range(len(args)):
        if argc <= j:
            res[args_name].append(args[j])
        else:
            res[arg_names[j]] = args[j]

    contains = set()
    for i in kwargs.keys():
        if i not in arg_names:
            if have_kwargs:
                res[kwargs_name][i] = kwargs[i]
            else:
                raise TypeError(ERR_TOO_MANY_KW_ARGS)
        elif arg_names.index(i) < pos:
            if have_kwargs:
                res[kwargs_name][i] = kwargs[i]
            else:
                raise TypeError(ERR_POSONLY_PASSED_AS_KW)
        elif i in contains or (i in res and i not in kw_defaults):
            raise TypeError(ERR_MULT_VALUES_FOR_ARG)
        else:
            contains.add(i)
            res[i] = kwargs[i]

    for j in range(len(defaults)):
        el = arg_names[argc - len(defaults) + j]
        if el not in res:
            res[el] = defaults[j]

    for i in kw_defaults.keys():
        if i not in res:
            res[i] = kw_defaults[i]

    for i in arg_names:
        if i not in res:
            if arg_names.index(i) < argc - len(kw_defaults):
                raise TypeError(ERR_MISSING_POS_ARGS)
            else:
                raise TypeError(ERR_MISSING_KWONLY_ARGS)

    for i in res:
        if i not in arg_names:
            raise TypeError(ERR_TOO_MANY_KW_ARGS)

    if have_args:
        res[args_name] = tuple(res[args_name])

    return res
