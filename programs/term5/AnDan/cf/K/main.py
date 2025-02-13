from bisect import bisect_left

if __name__ == '__main__':
    # Считываем количество объектов
    n = int(input())
    objects = []

    # Считываем координаты объектов (xi, yi)
    for i in range(n):
        x, y = map(int, input().split())
        objects.append((x, y))

    # Сортируем объекты по координате x
    objects.sort()

    # Считываем количество запросов
    m = int(input())
    queries = []

    # Считываем запросы (xq, kq)
    for i in range(m):
        x_q, k_q = map(int, input().split())
        queries.append((x_q, k_q))

    results = []

    # Обрабатываем каждый запрос
    for x_q, k_q in queries:
        # Находим позицию первого объекта с x >= x_q
        idx = bisect_left(objects, (x_q, -float('inf')))

        left, right = 0, 0
        l_ptr, r_ptr = idx - 1, idx
        combined = []

        # Собираем k ближайших объектов
        while (l_ptr >= 0 or r_ptr < n) and len(combined) < k_q:
            if l_ptr >= 0:
                dist_left = abs(objects[l_ptr][0] - x_q)
            else:
                dist_left = float('inf')

            if r_ptr < n:
                dist_right = abs(objects[r_ptr][0] - x_q)
            else:
                dist_right = float('inf')

            if dist_left <= dist_right:
                combined.append((dist_left, objects[l_ptr][1]))
                l_ptr -= 1
            else:
                combined.append((dist_right, objects[r_ptr][1]))
                r_ptr += 1

        # Если не удалось найти достаточно объектов, добавляем -1
        if len(combined) < k_q:
            results.append(-1.0)
            continue

        # Сортируем найденные объекты по расстоянию и по целевому признаку
        combined.sort()

        # Находим расстояние у k-го ближайшего объекта
        last_distance = combined[k_q - 1][0]

        # Проверяем, сколько объектов имеют такое же расстояние
        same_distance_count = sum(1 for d in combined if d[0] == last_distance)

        # Если количество объектов с одинаковым расстоянием больше k_q, то выводим -1
        if same_distance_count > k_q:
            results.append(-1.0)
        else:
            # Считаем среднее значение целевых признаков k ближайших объектов
            k_closest = combined[:k_q]
            average_y = sum(d[1] for d in k_closest) / k_q
            results.append(average_y)

    # Выводим результаты всех запросов
    print("\n".join(map(str, results)))
