import sys
import math
from typing import Any

PROMPT = '>>> '


def run_calc(context: dict[str, Any] | None = None) -> None:
    """Run interactive calculator session in specified namespace"""

    inp = sys.stdin
    out = sys.stdout

    while True:
        out.write(PROMPT)
        out.flush()
        expr = inp.readline()

        if not expr:
            out.write('\n')
            out.flush()
            break

        res = eval(expr, {"__builtins__": {}}, context)

        out.write("{}\n".format(str(res)))
        out.flush()


if __name__ == '__main__':
    context = {'math': math}
    run_calc(context)
