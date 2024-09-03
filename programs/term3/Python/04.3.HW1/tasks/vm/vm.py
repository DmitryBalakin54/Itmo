"""
Simplified VM code which works for some cases.
You need extend/rewrite code to pass all cases.
"""

import builtins
import dis
import types
import typing as tp
from typing import Any


CO_VARARGS = 4
CO_VARKEYWORDS = 8

ERR_TOO_MANY_POS_ARGS = 'Too many positional arguments'
ERR_TOO_MANY_KW_ARGS = 'Too many keyword arguments'
ERR_MULT_VALUES_FOR_ARG = 'Multiple values for arguments'
ERR_MISSING_POS_ARGS = 'Missing positional arguments'
ERR_MISSING_KWONLY_ARGS = 'Missing keyword-only arguments'
ERR_POSONLY_PASSED_AS_KW = 'Positional-only argument passed as keyword argument'


def bind_args(func: types.CodeType, *args: Any, **kwargs: Any) -> dict[str, Any]:
    """Bind values from `args` and `kwargs` to corresponding arguments of `func`

    :param func: function to be inspected
    :param args: positional arguments to be bound
    :param kwargs: keyword arguments to be bound
    :return: `dict[argument_name] = argument_value` if binding was successful,
             raise TypeError with one of `ERR_*` error descriptions otherwise
    """

    have_args = bool(CO_VARARGS & func.co_flags)
    have_kwargs = bool(CO_VARKEYWORDS & func.co_flags)

    argc = func.co_argcount
    names_count = argc + func.co_kwonlyargcount + have_args + have_kwargs
    arg_names = [*func.co_varnames][:names_count]

    res: dict[str, Any] = {}

    if names_count == 0:
        if not (len(args) == 0 or have_args):
            raise TypeError(ERR_TOO_MANY_POS_ARGS)
        return res

    if argc < len(args) and not have_args:
        raise TypeError(ERR_TOO_MANY_POS_ARGS)

    pos = func.co_posonlyargcount

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
        else:
            contains.add(i)
            res[i] = kwargs[i]

    for i in arg_names:
        if i not in res:
            raise TypeError(ERR_MISSING_KWONLY_ARGS)

    for i in res:
        if i not in arg_names:
            raise TypeError(ERR_TOO_MANY_KW_ARGS)

    if have_args:
        res[args_name] = tuple(res[args_name])

    return res


class Frame:
    """
    Frame header in cpython with description
        https://github.com/python/cpython/blob/3.11/Include/frameobject.h

    Text description of frame parameters
        https://docs.python.org/3/library/inspect.html?highlight=frame#types-and-members
    """
    def __init__(self,
                 frame_code: types.CodeType,
                 frame_builtins: dict[str, tp.Any],
                 frame_globals: dict[str, tp.Any],
                 frame_locals: dict[str, tp.Any],
                 frame_is_global: bool) -> None:
        self.code = frame_code
        self.builtins = frame_builtins
        self.globals = frame_globals
        self.locals = frame_locals
        self.data_stack: tp.Any = []
        self.return_value = None
        self.is_global = frame_is_global
        self.index = 0

    def top(self) -> tp.Any:
        return self.data_stack[-1]

    def pop(self) -> tp.Any:
        return self.data_stack.pop()

    def push(self, *values: tp.Any) -> None:
        self.data_stack.extend(values)

    def popn(self, n: int) -> tp.Any:
        """
        Pop a number of values from the value stack.
        A list of n values is returned, the deepest value first.
        """
        if n > 0:
            returned = self.data_stack[-n:]
            self.data_stack[-n:] = []
            return returned
        else:
            return []

    def run(self) -> tp.Any:
        instructions: list[dis.Instruction] = list(dis.get_instructions(self.code))
        strings: dict[int, int] = {instr.offset: ind for ind, instr in enumerate(instructions)}
        if self.index != 0 and instructions[self.index].opname != "RESUME":
            raise Exception
        while self.index < len(instructions):
            instr_name = instructions[self.index].opname.lower() + "_op"
            res = getattr(self, instr_name)(instructions, strings)
            self.index += 1
            if instr_name == "yield_value_op":
                return res
            if instr_name == "return_value_op":
                break
        return self.return_value

    def resume_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> tp.Any:
        pass

    def push_null_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> tp.Any:
        self.push(None)

    def precall_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> tp.Any:
        pass

    def call_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        """
        Operation description:
            https://docs.python.org/release/3.11.5/library/dis.html#opcode-CALL
        """
        arg = instructions[self.index]
        arg_v = arg.argval
        arguments = self.popn(arg_v)
        f = self.pop()
        try:
            self.push(f(*arguments))
        except TypeError:
            try:
                f()
                f.__init__(*arguments)
                self.push(f)
            except TypeError:
                new_f = self.pop()
                res = new_f()
                self.push(res)

    def binary_subscr_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        sl = self.pop()
        f = self.pop()
        self.push(f[sl])

    def store_subscr_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        sl = self.pop()
        f = self.pop()
        val = self.pop()
        f[sl] = val
        self.push(f)

    def delete_subscr_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        sl = self.pop()
        f = self.pop()
        del f[sl]

    def load_name_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        """
        Partial realization

        Operation description:
            https://docs.python.org/release/3.11.5/library/dis.html#opcode-LOAD_NAME
        """
        # TODO: parse all scopes
        arg = instructions[self.index]
        arg_v = arg.argval
        if len(self.data_stack) > 0 and self.top() is None:
            self.pop()
        try:
            el = self.locals[arg_v]
        except Exception:
            try:
                el = self.globals[arg_v]
            except Exception:
                try:
                    el = self.builtins[arg_v]
                except Exception:
                    raise NameError("name '" + arg_v + "' is not defined")
        self.push(el)

    def load_global_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        """
        Operation description:
            https://docs.python.org/release/3.11.5/library/dis.html#opcode-LOAD_GLOBAL
        """
        # TODO: parse all scopes
        arg = instructions[self.index]
        arg_v = arg.argval
        try:
            el = self.globals[arg_v]
        except Exception:
            try:
                el = self.builtins[arg_v]
            except Exception:
                raise NameError("name '" + arg_v + "' is not defined")
        self.push(el)

    def load_const_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        """
        Operation description:
            https://docs.python.org/release/3.11.5/library/dis.html#opcode-LOAD_CONST
        """
        arg = instructions[self.index]
        arg_v = arg.argval

        self.push(arg_v)

    def load_fast_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:

        arg = instructions[self.index]
        arg_v = arg.argval
        try:
            self.push(self.locals[arg_v])
        except Exception:
            raise UnboundLocalError("cannot access local variable '" + arg_v +
                                    "' where it is not associated with a value")

    def store_global_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        arg = instructions[self.index]
        arg_v = arg.argval
        self.globals[arg_v] = self.pop()

    def store_fast_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        arg = instructions[self.index]
        arg_v = arg.argval
        self.locals[arg_v] = self.pop()

    def store_attr_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        arg = instructions[self.index]
        arg_v = arg.argval
        setattr(self.pop(), arg_v, self.pop())

    def load_attr_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        arg = instructions[self.index]
        arg_v = arg.argval
        self.push(getattr(self.pop(), arg_v))

    def delete_attr_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        arg = instructions[self.index]
        arg_v = arg.argval
        delattr(self.pop(), arg_v)

    def delete_name_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        arg = instructions[self.index]
        arg_v = arg.argval
        del self.locals[arg_v]

    def delete_fast_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        arg = instructions[self.index]
        arg_v = arg.argval
        del self.locals[arg_v]

    def delete_global_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        arg = instructions[self.index]
        arg_v = arg.argval
        del self.globals[arg_v]

    def binary_op_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        arg = instructions[self.index]
        arg_r = arg.argrepr
        el_arg = self.pop()
        el = self.pop()
        if arg_r == "+=" or arg_r == "+":
            el += el_arg
        elif arg_r == "-=" or arg_r == "-":
            el -= el_arg
        elif arg_r == "*=" or arg_r == "*":
            el *= el_arg
        elif arg_r == "/=" or arg_r == "/":
            el /= el_arg
        elif arg_r == "%=" or arg_r == "%":
            el %= el_arg
        elif arg_r == "**=" or arg_r == "**":
            el **= el_arg
        elif arg_r == "//=" or arg_r == "//":
            el //= el_arg
        elif arg_r == "|=" or arg_r == "|":
            el |= el_arg
        elif arg_r == "&=" or arg_r == "&":
            el &= el_arg
        elif arg_r == "^=" or arg_r == "^":
            el ^= el_arg
        elif arg_r == "<<=" or arg_r == "<<":
            el <<= el_arg
        elif arg_r == ">>=" or arg_r == ">>":
            el >>= el_arg
        self.push(el)

    def compare_op_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        arg = instructions[self.index]
        arg_r = arg.argrepr
        el_arg = self.pop()
        el = self.pop()
        res = False
        if arg_r == "==":
            res = el == el_arg
        elif arg_r == "<":
            res = el < el_arg
        elif arg_r == "<=":
            res = el <= el_arg
        elif arg_r == ">":
            res = el > el_arg
        elif arg_r == ">=":
            res = el >= el_arg
        self.push(res)

    def unary_negative_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        self.push(-self.pop())

    def unary_positive_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        self.push(+self.pop())

    def unary_invert_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        self.push(~self.pop())

    def unary_not_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        self.push(not self.pop())

    def return_value_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        """
        Operation description:
            https://docs.python.org/release/3.11.5/library/dis.html#opcode-RETURN_VALUE
        """
        self.return_value = self.pop()

    def pop_top_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        """
        Operation description:
            https://docs.python.org/release/3.11.5/library/dis.html#opcode-POP_TOP
        """
        self.pop()

    def make_function_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        """
        Operation description:
            https://docs.python.org/release/3.11.5/library/dis.html#opcode-MAKE_FUNCTION
        """
        code = self.pop()  # the code associated with the function (at TOS1)
        # TODO: use arg to parse function defaults

        if code.co_name == '<genexpr>':
            def g(*args: tp.Any, **kwargs: tp.Any) -> tp.Any:
                # TODO: parse input arguments using code attributes such as co_argcount

                parsed_args: dict[str, tp.Any] = {}

                f_locals = dict(self.locals)
                f_locals.update(parsed_args)
                frame = Frame(code, self.builtins, self.globals, f_locals, False)
                res = frame.run()
                while True:
                    try:
                        new_res = frame.run()
                        yield res
                        res = new_res
                    except Exception:
                        break

            self.push(g)
            return

        def f(*args: tp.Any, **kwargs: tp.Any) -> tp.Any:
            # TODO: parse input arguments using code attributes such as co_argcount

            parsed_args: dict[str, tp.Any] = {}
            if not (code.co_name[0] == '<' and code.co_name[-1] == '>'):
                parsed_args = bind_args(code, *args, **kwargs)
            f_locals = dict(self.locals)
            f_locals.update(parsed_args)
            frame = Frame(code, self.builtins, self.globals, f_locals, False)  # Run code in prepared environment
            return frame.run()

        self.push(f)

    def store_name_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        """
        Operation description:
            https://docs.python.org/release/3.11.5/library/dis.html#opcode-STORE_NAME
        """
        arg = instructions[self.index]
        arg_v = arg.argval
        const = self.pop()
        self.locals[arg_v] = const
        if self.is_global:
            self.globals[arg_v] = const

    def get_iter_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        it = self.pop().__iter__()
        self.locals['.0'] = it
        self.push(it)

    def for_iter_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        arg = instructions[self.index]
        arg_v = arg.argval
        bound = strings[arg_v] - 1
        try:
            it = self.top().__next__()
            self.push(it)
        except Exception:
            self.pop()
            self.index = bound

    def jump_backward_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        arg = instructions[self.index]
        arg_v = arg.argval
        self.index = strings[arg_v] - 1

    def pop_jump_forward_if_true_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        arg = instructions[self.index]
        arg_v = arg.argval
        el = self.pop()
        if bool(el):
            self.index = strings[arg_v] - 1

    def pop_jump_forward_if_none_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        arg = instructions[self.index]
        arg_v = arg.argval
        el = self.pop()
        if el is None:
            self.index = strings[arg_v] - 1

    def jump_forward_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        arg = instructions[self.index]
        arg_v = arg.argval

        self.index = strings[arg_v] - 1

    def jump_if_true_or_pop_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        arg = instructions[self.index]
        arg_v = arg.argval
        el = self.top()
        if el:
            self.index = strings[arg_v] - 1
        else:
            self.pop()

    def jump_if_false_or_pop_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        arg = instructions[self.index]
        arg_v = arg.argval
        el = self.top()
        if not el:
            self.index = strings[arg_v] - 1
        else:
            self.pop()

    def pop_jump_forward_if_false_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        arg = instructions[self.index]
        arg_v = arg.argval
        el = self.pop()
        if not el:
            self.index = strings[arg_v] - 1

    def unpack_sequence_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        tup = self.pop()
        for el in reversed(tup):
            self.push(el)

    def load_assertion_error_op(self,  instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        self.push(AssertionError)

    def load_method_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        arg = instructions[self.index]
        arg_v = arg.argval
        sel = self.pop()
        self.push(sel.__getattribute__(arg_v))

    def load_build_class_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        self.push(self.builtins["__build_class__"])

    def raise_varargs_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        err = self.pop()
        raise err

    def build_slice_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        arg = instructions[self.index]
        arg_v = arg.argval
        params: list[int | None] = [None, None, None]
        for i in reversed(range(arg_v)):
            params[i] = self.pop()
        self.push(slice(*params))

    def build_list_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        arg = instructions[self.index]
        arg_v = arg.argval
        params: list[tp.Any] = []
        for i in range(arg_v):
            params.insert(0, self.pop())
        self.push(params)

    def build_map_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        arg = instructions[self.index]
        arg_v = arg.argval
        params: dict[tp.Any, tp.Any] = {}
        for i in range(arg_v):
            el = self.pop()
            params[self.pop()] = el
        self.push(params)

    def build_const_key_map_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        arg = instructions[self.index]
        arg_v = arg.argval
        params: list[tp.Any] = []
        tup = self.pop()
        params.extend([self.pop() for _ in range(arg_v)])
        self.push({tup[i]: params[-i - 1] for i in range(arg_v)})

    def build_set_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        self.push(set())

    def build_string_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        arg = instructions[self.index]
        arg_v = arg.argval
        strs: list[str] = [self.pop() for _ in range(arg_v)]
        self.push(''.join(i for i in reversed(strs)))

    def set_update_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        new_set = self.pop()
        self.top().update(new_set)

    def list_extend_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        new_seq = self.pop()
        self.top().extend(new_seq)

    def list_append_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        arg = instructions[self.index]
        arg_v = arg.argval
        el = self.pop()
        self.data_stack[-arg_v].append(el)

    def map_add_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        arg = instructions[self.index]
        arg_v = arg.argval
        el = self.pop()
        key = self.pop()
        self.data_stack[-arg_v][key] = el

    def set_add_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        arg = instructions[self.index]
        arg_v = arg.argval
        self.data_stack[-arg_v - 1].add(self.pop())

    def format_value_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        arg = instructions[self.index]
        arg_v = arg.argval

        if arg_v[0] is None:
            self.push(self.pop())
        else:
            self.push(arg_v[0](self.pop()))

    def return_generator_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        self.push(None)

    def yield_value_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> tp.Any:
        return self.top()

    def import_name_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        arg = instructions[self.index]
        arg_v = arg.argval
        els_tuple = self.pop()
        els = [*els_tuple] if els_tuple is not None else []
        self.push(__import__(arg_v, self.globals, self.locals, els, self.pop()))

    def import_from_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        arg = instructions[self.index]
        arg_v = arg.argval
        lib = self.top()
        self.push(lib.__getattribute__(arg_v))

    def import_star_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        lib = self.pop()
        for i in dir(lib)[6:]:
            self.locals[i] = getattr(lib, i)
            if self.is_global:
                self.globals[i] = getattr(lib, i)

    def kw_names_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        pass

    def copy_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        arg = instructions[self.index]
        arg_v = arg.argval
        self.push(self.data_stack[-arg_v])

    def swap_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        arg = instructions[self.index]
        arg_v = arg.argval
        self.data_stack[-arg_v], self.data_stack[-1] = self.data_stack[-1], self.data_stack[-arg_v]

    def is_op_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        arg = instructions[self.index]
        arg_v = arg.argval

        if arg_v == 1:
            self.push(self.pop() is not self.pop())
        else:
            self.push(self.pop() is self.pop())

    def nop_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        pass

    def contains_op_op(self, instructions: list[dis.Instruction], strings: dict[int, int]) -> None:
        arg = instructions[self.index]
        arg_v = arg.argval
        seq = self.pop()
        el = self.pop()
        if arg_v:
            self.push(el not in seq)
        else:
            self.push(el in seq)


class VirtualMachine:
    def run(self, code_obj: types.CodeType) -> None:
        """
        :param code_obj: code for interpreting
        """
        globals_context: dict[str, tp.Any] = {}
        frame = Frame(code_obj, builtins.globals()['__builtins__'], globals_context, globals_context, True)
        return frame.run()
