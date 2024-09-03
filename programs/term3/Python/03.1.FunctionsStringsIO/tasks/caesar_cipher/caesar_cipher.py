import string


def caesar_encrypt(message: str, n: int) -> str:
    """Encrypt message using caesar cipher

    :param message: message to encrypt
    :param n: shift
    :return: encrypted message
    """

    letters_amount = len(string.ascii_lowercase)
    res: str = ''

    for ch in message:
        if ch not in string.ascii_letters:
            res += ch
            continue

        begin = ord('a')

        if ch == ch.upper():
            begin = ord('A')

        code = ord(ch) - begin
        code = (code + n) % letters_amount
        res += chr(code + begin)

    return res
