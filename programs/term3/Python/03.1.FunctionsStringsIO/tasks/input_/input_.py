import sys
import typing as tp


def input_(prompt: str | None = None,
           inp: tp.IO[str] | None = None,
           out: tp.IO[str] | None = None) -> str | None:
    """Read a string from `inp` stream. The trailing newline is stripped.

    The `prompt` string, if given, is printed to `out` stream without a
    trailing newline before reading input.

    If the user hits EOF (*nix: Ctrl-D, Windows: Ctrl-Z+Return), return None.

    `inp` and `out` arguments are optional and should default to `sys.stdin`
    and `sys.stdout` respectively.
    """

    inp_stream = sys.stdin if inp is None else inp
    out_stream = sys.stdout if out is None else out

    if prompt is not None:
        out_stream.write(prompt)
        out_stream.flush()

    res = inp_stream.readline()
    res = res[0:len(res) - 1]

    return res if res != '' else None
