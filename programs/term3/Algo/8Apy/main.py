def prefix_function(s):
    p = [0] * len(s)
    for i in range(1, len(s)):
        p[i] = p[i - 1]
        while p[i] > 0 and s[i] != s[p[i]]:
            p[i] = p[p[i] - 1]
        if s[i] == s[p[i]]:
            p[i] += 1
    return p


for value in prefix_function(input().strip()):
    print(value, end=" ")
