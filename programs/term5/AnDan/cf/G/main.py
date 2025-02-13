import numpy as np

def gini_index(n, k, classes):
    total_count = np.zeros(k + 1, dtype=int)
    for c in classes:
        total_count[c] += 1

    left_count = np.zeros(k + 1, dtype=int)
    left_size = 0
    right_size = n

    total_gini = []

    for i in range(n - 1):
        c = classes[i]

        left_count[c] += 1
        total_count[c] -= 1
        left_size += 1
        right_size -= 1

        if left_size > 0:
            left_probs = left_count / left_size
            gini_left = 1 - np.sum(left_probs ** 2)
        else:
            gini_left = 0

        if right_size > 0:
            right_probs = total_count / right_size
            gini_right = 1 - np.sum(right_probs ** 2)
        else:
            gini_right = 0

        gini = (left_size / n) * gini_left + (right_size / n) * gini_right
        total_gini.append(gini)

    return total_gini


if __name__ == '__main__':
    n, k = map(int, input().split())
    classes = list(map(int, input().split()))
    result = gini_index(n, k, classes)
    for x in result:
        print(x)
