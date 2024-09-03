import math
from math import fabs, asin, cos, sin, pi
from time import time

import matplotlib.collections
import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle

deltas = [0.1, 0.01, 0.001]
radius = 1
int_val = 2 / (3 * math.sqrt(2))


# Пункт 2.2
def f(_, y):
    return y


def center_in(x, y, _):
    return x ** 2 + y ** 2 <= radius ** 2 and x >= y


def all_in(x, y, d):
    shifts = [d / 2, -d / 2]
    for shift_x in shifts:
        for shift_y in shifts:
            if (x + shift_x) ** 2 + (y + shift_y) ** 2 > radius ** 2 or (x + shift_x) < (y + shift_y):
                return False
    return True


def corner_in(x, y, d):
    shifts = [d / 2, -d / 2]
    for shift_x in shifts:
        for shift_y in shifts:
            if (x + shift_x) ** 2 + (y + shift_y) ** 2 <= radius ** 2 and (x + shift_x) >= (y + shift_y):
                return True
    return False


def draw_partition(type, delta, all_rects):
    plt.figure(figsize=(10, 10))
    plt.axhline(0, color='black', linewidth=0.5)
    plt.axvline(0, color='black', linewidth=0.5)
    plt.xlabel('x'), plt.ylabel('y')
    plt.title(f'Разбиение множества для d = {delta} ({type})')
    plt.gca().set(xlim=(-1.5, 1.5), ylim=(-1.5, 1.5))

    plt.gca().add_collection(matplotlib.collections.PatchCollection(all_rects))


# Переписанный участок кода для сохранения каждого графика отдельно
for idx, d in enumerate(deltas):
    start_time = time()
    int_sum = 0
    min_sum = 0
    max_sum = 0
    squares = []
    plot_squares = []
    plot_squares_all = []
    plot_squares_corn = []
    plot_squares_cent = []

    cur_y = 1 / math.sqrt(2) + d
    step_y = 0
    while cur_y - step_y * d > -radius - 0.5:
        cur_x = -1 / math.sqrt(2) - d
        step_x = 0
        while fabs(cur_x + step_x * d) < radius + 1.5:
            x = cur_x + step_x * d + d / 2
            y = cur_y - step_y * d - d / 2

            if not (all_in(x, y, d) or center_in(x, y, d) or corner_in(x, y, d)):
                step_x += 1
                continue

            if all_in(x, y, d):
                min_sum += d ** 2 * f(x, y)

            if center_in(x, y, d):
                int_sum += d ** 2 * f(x, y)

            if corner_in(x, y, d):
                max_sum += d ** 2 * f(x, y)

            # squares.append((x, y))
            # plot_squares.append(Rectangle((x - d / 2, y - d / 2), d, d))
            # int_sum += countIntSum_cond(x, y, d)
            # min_sum += countMinSum_cond(x, y, d)
            # max_sum += countMaxSum_cond(x, y, d)
            step_x += 1
        step_y += 1

    int_sum = -int_sum
    print(f"Результат при d = {d}: ")
    print(
        f"Инт. сумма: ", int_sum,
        ", Δintegral: ", fabs(int_val - int_sum),
        ", Δtime: ", time() - start_time,
        ", Δ(max - min): ", max_sum - min_sum
    )

    # plt.show()

print('end')
