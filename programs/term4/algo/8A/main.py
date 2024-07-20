import cmath


def solve(a, b):

    if a[0] == '0' or b[0] == '0':
        print('0')
        return 

    arr = [int(digit) for digit in (a[1:] if a[0] == '-' else a)]
    brr = [int(digit) for digit in (b[1:] if b[0] == '-' else b)]
    negative = (a[0] == '-') ^ (b[0] == '-')

    result = multiply(arr, brr)
    result_str = convert(result)
    if result_str == '':
        result_str = '1'
    if negative:
        result_str = "-" + result_str
    print(result_str)


def multiply(a, b):
    max_length = max(len(a), len(b))
    n = 1
    while n < max_length:
        n *= 2
    n *= 2

    aa = complex_array(n)
    bb = complex_array(n)

    for i in range(len(a)):
        aa[i] = complex(a[i], 0)
    for i in range(len(b)):
        bb[i] = complex(b[i], 0)

    w = cmath.exp(2j * cmath.pi / n)
    ya = fft(aa, w)
    yb = fft(bb, w)
    yc = [ya[i] * yb[i] for i in range(len(ya))]
    return reverse_fft(yc, w)


def convert(a):
    a = trim(a)
    carry = 0
    res = []

    for i in range(len(a) - 1, -1, -1):
        a[i] += carry
        res.append(a[i] % 10)
        carry = a[i] // 10

    if carry != 0:
        res.append(carry)

    return ''.join(map(str, res[::-1]))


def trim(a):
    i = len(a) - 1
    while i >= 0 and a[i] == 0:
        i -= 1

    return a[:i + 1] if i != -1 else []


def complex_array(n):
    return [0] * n


def fft(a, w):
    n = len(a)
    if n == 1:
        return a

    a0 = a[0:n:2]
    a1 = a[1:n:2]
    w_squared = w * w
    y0 = fft(a0, w_squared)
    y1 = fft(a1, w_squared)
    y = [0] * n
    wi = 1

    for i in range(n // 2):
        y[i] = y0[i] + wi * y1[i]
        y[i + n // 2] = y0[i] - wi * y1[i]
        wi *= w

    return y


def reverse_fft(a, w):
    n = len(a)
    b = fft(a, w)
    for i in range(1, n // 2):
        b[i], b[n - i] = b[n - i], b[i]

    return [round(b[i].real / n) for i in range(n)]


if __name__ == "__main__":
    a = input()
    b = input()
    solve(a, b)
