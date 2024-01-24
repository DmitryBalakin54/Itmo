def z_function(s):
    n = len(s)
    z_values = [0] * n
    l, r = 0, 0
    for i in range(1, n):
        if i <= r:
            z_values[i] = min(r - i + 1, z_values[i - l])
        while i + z_values[i] < n and s[z_values[i]] == s[i + z_values[i]]:
            z_values[i] += 1
        if i + z_values[i] - 1 > r:
            l, r = i, i + z_values[i] - 1
    return z_values


def find_occurrences(a, b):
    n = len(a)
    k = len(b)

    combined = b + [0] + a

    z_values = z_function(combined)

    occurrences = []
    for i in range(k + 1, n + k + 1):
        if z_values[i] == k:
            occurrences.append(i - k - 1)

    return occurrences


if __name__ == "__main__":
    n, k = map(int, input().split())
    a = list(map(int, input().split()))
    b = list(map(int, input().split()))

    result = find_occurrences(a, b)

    print(" ".join(map(str, result)))
