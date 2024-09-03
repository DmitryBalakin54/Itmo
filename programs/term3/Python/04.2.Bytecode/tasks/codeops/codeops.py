import types
import dis
from collections import defaultdict


def count_operations(source_code: types.CodeType) -> dict[str, int]:
    """Count byte code operations in given source code.

    :param source_code: the bytecode operation names to be extracted from
    :return: operation counts
    """

    res: dict[str, int] = defaultdict(int)
    for i in dis.get_instructions(source_code):
        if isinstance(i.argval, types.CodeType):
            for key, val in count_operations(i.argval).items():
                res[key] += val
        res[i.opname] += 1

    return dict(res)
