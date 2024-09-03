import typing as tp
from pathlib import Path


def tail(filename: Path, lines_amount: int = 10, output: tp.IO[bytes] | None = None) -> None:
    """
    :param filename: file to read lines from (the file can be very large)
    :param lines_amount: number of lines to read
    :param output: stream to write requested amount of last lines from file
                   (if nothing specified stdout will be used)
    """
