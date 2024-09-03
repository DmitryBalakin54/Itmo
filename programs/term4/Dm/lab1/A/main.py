MOD = 998244353


def main():
    _, _ = map(int, input().strip().split())
    p = list(map(int, input().strip().split()))
    q = list(map(int, input().strip().split()))

    sum_r = poly_sum(p, q)
    print_p(sum_r)

    prod_r = pp(p, q)
    print_p(prod_r)

    div_r = poly_div(p, q)
    print_p(div_r, 1000, False)


def poly_sum(p, q):
    sz = max(len(p), len(q))
    res = [0] * sz
    for i in range(sz):
        res[i] = (get(i, p) + get(i, q)) % MOD
    return res


def pp(p, q):
    n = len(p) + len(q)
    res = [0] * n
    for i in range(n):
        c = 0
        for j in range(i + 1):
            c += get(j, p) * get(i - j, q)
            c %= MOD
        res[i] = c
    return res


def poly_div(p, q):
    n = 1000
    res = [0] * n
    for i in range(n):
        b = 0
        for j in range(1, i + 1):
            b += get(j, q) * get(i - j, res)
            b %= MOD
        res[i] = (get(i, p) - b) * pow(q[0], -1, MOD)
        res[i] %= MOD
    return res


def print_p(a, amount=None, flag=True):
    power = next((i for i in range(len(a) - 1, -1, -1) if a[i] != 0), 0)
    if flag:
        print(power)
    amount = amount if amount is not None else power + 1
    for i in range(amount):
        print(get(i, a), end=" ")
    print()


def get(i, a):
    return a[i] if i < len(a) else 0


if __name__ == "__main__":
    main()
