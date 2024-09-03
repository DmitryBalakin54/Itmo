def get_fizz_buzz(n: int) -> list[int | str]:
    """
    If value divided by 3 - "Fizz",
       value divided by 5 - "Buzz",
       value divided by 15 - "FizzBuzz",
    else - value.
    :param n: size of sequence
    :return: list of values.
    """
    a = 'FizzBuzz'
    b = 'Fizz'
    c = 'Buzz'
    return [a if i % 3 == 0 and i % 5 == 0 else b if i % 3 == 0 else c if i % 5 == 0 else i for i in range(1, n + 1)]
