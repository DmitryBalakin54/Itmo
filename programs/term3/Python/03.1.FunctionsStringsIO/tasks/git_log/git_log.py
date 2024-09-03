import typing as tp


def reformat_git_log(inp: tp.IO[str], out: tp.IO[str]) -> None:
    """Reads git log from `inp` stream, reformats it and prints to `out` stream

    Expected input format: `<sha-1>\t<date>\t<author>\t<email>\t<message>`
    Output format: `<first 7 symbols of sha-1>.....<message>`
    """

    for log in inp.read().split('\n'):
        if not len(log):
            continue

        log_lst = log.split('\t')
        tail = log_lst[-1]
        head = log_lst[0][0:7]
        dots = '.' * (80 - len(head) - len(tail))

        out.write("{}{}{}\n".format(head, dots, tail))
