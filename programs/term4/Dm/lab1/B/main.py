MOD = 998244353
fact = {}
pow4 = [0] * 105


def ini():
    pow4[0] = 1
    for i in range(1, len(pow4)):
        pow4[i] = mod(pow4[i - 1] * 4)


def fact_f(a):
    if a not in fact:
        res = 1
        for i in range(2, a + 1):
            res *= i
            res = mod(res)
        fact[a] = res
    return fact[a]


def print_p(a, amount):
    for i in range(amount):
        print(get(i, a), end=" ")
    print()


def main():
    ini()
    n, m = map(int, input().strip().split())
    p = list(map(int, input().strip().split()))

    res_sqrt = ans(p, m, sqrt)
    print_p(res_sqrt, m)

    res_e = ans(p, m, e)
    print_p(res_e, m)

    res_ln = ans(p, m, ln)
    print_p(res_ln, m)


def ans(p, m, fff):
    res = [0]
    p_to_i = [1]
    for i in range(m):
        res = poly_sum(res, fff(p_to_i, i))
        p_to_i = poly_prod(p_to_i, p, m)
    return res


def sqrt(a, n):
    neg = -1 if n % 2 == 1 else 1
    nom = mod(fact_f(2 * n) * neg)
    den = mul(pow4[n], fact_f(n), fact_f(n), 1 - 2 * n)
    return poly_div(poly_mul(a, nom), den)


def mul(*args):
    res = args[0]
    for i in range(1, len(args)):
        res = mod(res * args[i])
    return res


def e(a, n):
    return poly_div(a, fact_f(n))


def ln(a, n):
    if n == 0:
        return []
    res = poly_div(a, n)
    if n % 2 == 0:
        negate(res)
    return res


def poly_mul(a, h):
    mul = []
    for i in range(len(a)):
        mul.append(mod(a[i] * h))
    return mul


def poly_sum(p, q):
    size = max(len(p), len(q))
    sum_res = [0] * size
    for i in range(size):
        sum_res[i] = get(i, p) + get(i, q)
        sum_res[i] = mod(sum_res[i])
    return sum_res


def poly_prod(p, q, m):
    prod = [0] * m
    for i in range(m):
        c = 0
        for j in range(i + 1):
            c += get(j, p) * get(i - j, q)
            c = mod(c)
        prod[i] = c
    return prod


def poly_div(p, a):
    div = []
    for i in range(len(p)):
        div.append(div_func(p[i], a))
    return div


def div_func(a, b):
    return mod(a * rev(b))


def rev(a):
    p = Pair(0, 0)
    g = gcd(a, MOD, p)
    if g != 1:
        raise AssertionError()
    else:
        return mod(p.x)


def negate(a):
    for i in range(len(a)):
        a[i] = mod(-a[i])


def gcd(a, b, p):
    if a == 0:
        p.x = 0
        p.y = 1
        return b
    p1 = Pair(0, 0)
    d = gcd(b % a, a, p1)
    p.x = mod(p1.y - (b // a) * p1.x)
    p.y = mod(p1.x)
    return mod(d)


def mod(a):
    a %= MOD
    if a < 0:
        a += MOD
    return a


def get(i, a):
    return a[i] if i < len(a) else 0


class Pair:
    def __init__(self, x, y):
        self.x = x
        self.y = y


if __name__ == "__main__":
    main()
