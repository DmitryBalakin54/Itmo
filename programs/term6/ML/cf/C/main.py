import numpy as np


def read_mat(sz):
    return [list(map(int, input().split())) for _ in range(sz)]


def read_szs():
    return map(int, input().split())


n, m = read_szs()
k = n - m + 1
A = np.array(read_mat(n))
B = np.array(read_mat(m))

for r in \
        np.linalg.lstsq(
            np.lib.stride_tricks.as_strided(A, shape=(m, m, k, k), strides=A.strides * 2).reshape(m ** 2, k ** 2),
            B.flatten(), rcond=None)[0].reshape(k, k):
    for x in r:
        print(x, end=' ')
    print()
