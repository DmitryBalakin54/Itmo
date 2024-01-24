def build_suffix_array(s):
    n = len(s)
    suffixes = [(s[i:], i) for i in range(n)]
    suffixes.sort()

    suffix_array = [suffix[1] for suffix in suffixes]
    return suffix_array

def count_lexicographically_ordered_pairs(s):
    n = len(s)
    suffix_array = build_suffix_array(s)

    result = 0
    for i in range(1, n):
        lcp = 0
        while (i + lcp < n) and (suffix_array[i - 1] + lcp < n) and (s[i + lcp] == s[suffix_array[i - 1] + lcp]):
            lcp += 1
        result += n - suffix_array[i] - lcp

    return result

# Пример использования
s = input().strip()
result = count_lexicographically_ordered_pairs(s)
print(result)
