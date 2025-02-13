def calculate_distances(data):
    classes = {}
    for x, y in data:
        if y not in classes:
            classes[y] = []
        classes[y].append(x)

    intra_class_distance = 0
    for values in classes.values():
        if not sorted(values) == values and not sorted(values, reverse=True) == values:
            values.sort(reverse=True)
        n = len(values)
        m = n - 1
        a = 0
        b = 0
        for i in range(n - 1):
            a += values[i] * (m - i)
            b += values[i + 1] * (i + 1)
        intra_class_distance += abs(a - b)

    all_values = [x for x, _ in data]
    if not sorted(all_values) == all_values and not sorted(all_values, reverse=True) == all_values:
        all_values.sort(reverse=True)
    n = len(all_values)
    m = n - 1
    a = 0
    b = 0
    for i in range(n - 1):
        a += all_values[i] * (m - i)
        b += all_values[i + 1] * (i + 1)
    total_distance = abs(a - b)

    inter_class_distance = total_distance - intra_class_distance
    return 2 * intra_class_distance, 2 * inter_class_distance


if __name__ == '__main__':
    k = int(input())
    n = int(input())
    data = [tuple(map(int, input().split())) for _ in range(n)]
    intra, inter = calculate_distances(data)
    print(f"{intra}\n{inter}")
