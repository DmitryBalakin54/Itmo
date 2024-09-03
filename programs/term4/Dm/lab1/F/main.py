MOD = int(1e9 + 7)

def solve(lst):
    lst.pop(0)
    for el in lst:
        print(el % MOD, end=" ")

def count_trees(max_value, coefficients):
    vals = [0] * max_value
    ans = [0] * (len(vals) + 1)
    vals[0], ans[0] = 1, 1

    for it in range(max_value):
        value = 0
        for iteration in coefficients:
            if it - iteration + 1 >= 0:
                value = (value + vals[it - iteration + 1]) % MOD

        ans[it + 1] = value
        for jnd in range(1, it + 3):
            if jnd + it + 1 <= max_value:
                ind = jnd + it
                kk = not (jnd - 1 == it + 1)

                for h in range(kk + 1):
                    vals[ind] = (vals[ind] + value * ans[jnd - 1]) % MOD

    return ans

if __name__ == "__main__":
    num_terms, max_value = map(int, input().split())
    coefficients = list(map(int, input().split()))

    result = count_trees(max_value, coefficients)
    solve(result)
