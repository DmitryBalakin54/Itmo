def count_util(text: str, flags: str | None = None) -> dict[str, int]:
    """
    :param text: text to count entities
    :param flags: flags in command-like format - can be:
        * -m stands for counting characters
        * -l stands for counting lines
        * -L stands for getting length of the longest line
        * -w stands for counting words
    More than one flag can be passed at the same time, for example:
        * "-l -m"
        * "-lLw"
    Ommiting flags or passing empty string is equivalent to "-mlLw"
    :return: mapping from string keys to corresponding counter, where
    keys are selected according to the received flags:
        * "chars" - amount of characters
        * "lines" - amount of lines
        * "longest_line" - the longest line length
        * "words" - amount of words
    """

    keys: set[str] = {ch for ch in 'mLlw' if not flags or ch in flags}

    strs: list[str] = text.split('\n')

    print(strs)

    res: dict[str, int] = {}

    if 'm' in keys:
        res['chars'] = len(text)

    if 'l' in keys:
        res['lines'] = len(strs) - 1

    if 'L' in keys:
        res['longest_line'] = max([len(s) for s in strs])

    if 'w' in keys:
        res['words'] = len([word for s in strs for word in s.split()])

    return res
