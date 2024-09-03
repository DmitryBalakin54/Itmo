import math
from math import fabs, asin, cos, sin, pi
from time import time

import matplotlib.collections
import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle

deltas = [0.001]
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


    plot_squares_all = []

    cur_y = 1 / math.sqrt(2) + d
    step_y = 0
    while cur_y - step_y * d > -radius - 0.5:
        cur_x = -1 / math.sqrt(2) - d
        step_x = 0
        while fabs(cur_x + step_x * d) < radius + 1.5:
            x = cur_x + step_x * d + d / 2
            y = cur_y - step_y * d - d / 2

            if all_in(x, y, d):
                rect = Rectangle((x - d / 2, y - d / 2), d, d)
                plot_squares_all.append(rect)

            step_x += 1
        step_y += 1

    # Сохранение графиков с уникальными именами
    draw_partition('обычная сумма', d, plot_squares_all)
    plt.savefig(f'graph2_{idx + 1}_d_{d}_2.png')

    plt.close()
