import math
import multiprocessing
import subprocess
import sys
import threading
from math import fabs, asin, cos, sin, pi
import time


import matplotlib.collections
import matplotlib.pyplot as plt

# from matplotlib.patches import Rectangle

start_prog_time = time.time()
deltas = [0.1, 0.01, 0.001]
radius = 1
int_val = 2 / (3 * math.sqrt(2))


# Пункт 2.1
def p(x, y):
    return x ** 2 + y ** 2


def q(x, y):
    return 3 * x * y


def param_x(phi):
    return radius * cos(phi)


def param_y(phi):
    return radius * sin(phi)


def addDots(dots, new_dots):
    for dot in new_dots:
        dots.append(dot)


def countIntSum(partition):
    res = 0
    for i in range(1, len(partition)):
        x = partition[i][0]
        y = partition[i][1]
        dx = x - partition[i - 1][0]
        dy = y - partition[i - 1][1]
        res += p(x, y) * dx + q(x, y) * dy
    return res


# Переписанный участок кода для сохранения каждого графика отдельно
for idx, d in enumerate(deltas):
    start_time = time.time()
    int_sum = 0
    dots = []

    # сначала посчитаем интегральную сумму на линии x = 0, y \in [-3, 3]
    partition = []
    step_x = d
    step_y = d
    start_x = -1 / math.sqrt(2)
    start_y = -1 / math.sqrt(2)

    step = 0
    partition.append((start_x, start_y))
    while fabs((start_x + step * step_x) ** 2 + (start_y + step * step_y) ** 2) < radius:
        partition.append((start_x + step_x * step, start_y + step_y * step))
        step += 1
    partition.append((1 / math.sqrt(2), 1 / math.sqrt(2)))
    int_sum += countIntSum(partition)
    addDots(dots, partition)

    # теперь считаем интегральную сумму на полуокружности
    partition = []
    step_phi = 2 * asin(d / (2 * radius))
    start_phi = -3 * pi / 4

    step = 0
    partition.append((start_x, start_y))
    while start_phi + step * step_phi < pi / 4:
        partition.append((
            param_x(start_phi + step * step_phi),
            param_y(start_phi + step * step_phi)
        ))
        step += 1
    partition.append((param_x(pi / 4), param_y(pi / 4)))
    int_sum += countIntSum(partition)
    addDots(dots, partition)

    print(f"Результат при d = {d}: ")
    print(
        f"Инт. сумма: ", int_sum,
        ", Δintegral: ", fabs(int_val - int_sum),
        ", Δtime: ", time.time() - start_time
    )

    plt.figure(figsize=(10, 10))
    partition_x = [dot[0] for dot in dots]
    partition_y = [dot[1] for dot in dots]
    plt.plot(partition_x, partition_y, marker='.', linestyle='-', label=f'd = {d}', markersize=3)
    plt.xlabel('x'), plt.ylabel('y')
    plt.gca().set(xlim=(-1.5, 1.5), ylim=(-1.5, 1.5))
    plt.title(f'Разбиение кривой для d = {d}')
    plt.grid()
    plt.gca().set_aspect('equal')
    plt.legend()

    # Сохранение графика с уникальным именем
    plt.savefig(f'graph_{idx + 1}_d_{d}.png')
    plt.close()


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


def countIntSum_cond(x, y, d):
    res = 0
    if center_in(x, y, d):
        res += d ** 2 * f(x, y)
    return res


def countMinSum_cond(x, y, d):
    res = 0
    if all_in(x, y, d):
        res += d ** 2 * f(x, y)
    return res


def countMaxSum_cond(x, y, d):
    res = 0
    if corner_in(x, y, d):
        res += d ** 2 * f(x, y)
    return res


def draw_partition(type, delta, all_rects):
    plt.figure(figsize=(10, 10))
    plt.axhline(0, color='black', linewidth=0.5)
    plt.axvline(0, color='black', linewidth=0.5)
    plt.xlabel('x'), plt.ylabel('y')
    plt.title(f'Разбиение множества для d = {delta} ({type})')
    plt.gca().set(xlim=(-1.5, 1.5), ylim=(-1.5, 1.5))

    # for i, (x, y) in enumerate(squares):
    #     if not cond(x, y, d):
    #         continue
    #     #rect = Rectangle((x - delta / 2, y - delta / 2), delta, delta)
    #     #collection.append(rect)
    #     collection.append(all_rects[i])
    plt.gca().add_collection(matplotlib.collections.PatchCollection(all_rects))


dir = str(__file__)[:str(__file__).rindex('\\')]


def fff(lst):
    subprocess.Popen(lst, cwd=dir)


with multiprocessing.Pool() as pool:
    # pool.map(fff, (i for i in
    #                [[sys.executable, 'proc0.py'],
    #                 [sys.executable, 'proc1.py'],
    #                 [sys.executable, 'proc2.py'],
    #                 [sys.executable, 'proc3.py']]))
    pool.apply_async(subprocess.Popen([sys.executable, 'proc0.py'], cwd=dir))
    pool.apply_async(subprocess.Popen([sys.executable, 'proc1.py'], cwd=dir))
    pool.apply_async(subprocess.Popen([sys.executable, 'proc2.py'], cwd=dir))
    pool.apply_async(subprocess.Popen([sys.executable, 'proc3.py'], cwd=dir))

time.sleep(60)
exit(0)

threads = [threading.Thread(target=subprocess.Popen, args=(i,), kwargs={'cwd': dir}) for i in
           [
            [sys.executable, 'proc1.py'],
            [sys.executable, 'proc2.py'],
            [sys.executable, 'proc3.py']]]

for thread in threads:
    thread.start()

for thread in threads:
    thread.join()

exit(0)
proc1 = subprocess.Popen(['python', dir + '\\proc1'])
proc2 = subprocess.Popen(['python', dir + '\\proc2'])
proc3 = subprocess.Popen(['python', dir + '\\proc3'])
proc0 = subprocess.Popen(['python', dir + '\\proc0'])

exit(0)

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

print(f'time: {time() - start_prog_time}')
print('end')
