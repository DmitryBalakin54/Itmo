def n_function(s):
    last, bound = 0, 0
    z = [0] * len(s)
    for i in range(1, len(s)):
        if i < bound:
            z[i] = max(0, min(z[i - last], bound - i))
        while i + z[i] < len(s) and s[z[i]] == s[i + z[i]]:
            z[i] += 1
        if i + z[i] > bound:
            last = i
            bound = i + z[i]
    z[0] = len(s)
    return z


n = int(input())
s = input()
s += s[::-1]

res = n_function(s)

for i in range(len(s) - 1, n - 1, -1):
    print(res[i], end=" ")


