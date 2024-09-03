import math

r_pow = []


def f(k, i, p):
    res = [1]
    f = [0, 1]
    for j in range(k):
        f[0] = j + 1 - i
        res = prd(res, f)
    return mul(res, r_pow[k - i] * p[i])


def prd(p, q):
    res = []
    for i in range(len(p) + len(q)):
        c = 0
        for j in range(i + 1):
            c += (p[j] if j < len(p) else 0) * (q[i - j] if i - j < len(q) else 0)
        res.append(c)
    return res


def sum_p(p, q):
    res = []
    for i in range(max(len(p), len(q))):
        res.append((p[i] if i < len(p) else 0) + (q[i] if i < len(q) else 0))
    return res


def mul(a, h):
    res = []
    for i in range(len(a)):
        res.append(a[i] * h)
    return res


if __name__ == "__main__":
    r, k = map(int, input().strip().split())
    p = list(map(int, input().strip().split()))

    r_pow = [1]
    for i in range(1, 11):
        r_pow.append(r * r_pow[i - 1])

    res = []
    for i in range(1, k + 2):
        res = sum_p(res, f(k, i - 1, p))

    d = r_pow[k] * math.factorial(k)
    for i in range(k + 1):
        nom = res[i]
        result = math.gcd(abs(nom), abs(d))
        print(f"{nom // result}/{d // result}", end=" ")
