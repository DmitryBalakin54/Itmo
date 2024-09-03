import subprocess
from pathlib import Path


def python_sort(file_in: Path, file_out: Path) -> None:
    """
    Sort tsv file using python built-in sort
    :param file_in: tsv file to read from
    :param file_out: tsv file to write to
    """

    with open(file_in, 'r') as inp:
        with open(file_out, 'w') as out:
            out.writelines(sorted(inp.readlines(), key=(lambda s: (int(s.split('\t')[1]), s.split('\t')[0]))))


def util_sort(file_in: Path, file_out: Path) -> None:
    """
    Sort tsv file using sort util
    :param file_in: tsv file to read from
    :param file_out: tsv file to write to
    """

    with open(file_in, 'r') as inp:
        proc = subprocess.Popen(['sort', '-t', '\t', '-k2,2n', '-k1'], stdout=subprocess.PIPE, stdin=inp)
        res, _ = proc.communicate()
        with open(file_out, 'w') as out:
            out.writelines(res.decode())
