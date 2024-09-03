def find_occurrences(s, t):
    occurrences = []
    index = s.find(t)
    while index != -1:
        occurrences.append(index)
        index = s.find(t, index + 1)
    return occurrences


print(" ".join(map(str, find_occurrences(input().strip(), input().strip()))))
