def make_p(a, c):
    n = len(a)
    res = [0] * n
    for k in range(n):
        ac = 0
        for i in range(1, n + 1):
            ac += c[i - 1] * get(k - i, a)
        res[k] = a[k] - ac
    return res


def get(i, pol):
    return pol[i] if 0 <= i < len(pol) else 0


def print_pol(pol):
    power = next((i for i in range(len(pol) - 1, -1, -1) if pol[i] != 0), 0)
    print(power)
    for i in range(power + 1):
        print(get(i, pol), end=" ")
    print()


if __name__ == "__main__":
    line = input().strip().split()
    k = int(line[0])
    a = list(map(int, input().strip().split()))
    c = list(map(int, input().strip().split()))

    q = [1] + [-c[i] for i in range(k)]
    res = make_p(a, c)
    print_pol(res)
    print_pol(q)
