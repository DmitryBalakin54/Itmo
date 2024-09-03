def get_middle_value(a: int, b: int, c: int) -> int:
    """
    Takes three values and returns middle value.
    """

    return a if b <= a <= c or c <= a <= b else b if a <= b <= c or c <= b <= a else c
