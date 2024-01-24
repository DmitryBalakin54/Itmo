def prefix_function(s):
    p = [0] * len(s)
    for i in range(1, len(s)):
        j = p[i - 1]
        while j > 0 and s[i] != s[j]:
            j = p[j - 1]
        if s[i] == s[j]:
            j += 1
        p[i] = j
    return p


def find_shift(k, d):
    if len(k) != len(d) or k not in d + d:
        return -1

    return (d + d).index(k)



print(find_shift(input().strip(), input().strip()))
