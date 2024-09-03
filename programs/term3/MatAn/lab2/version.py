import matplotlib.collections
import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle

from math import sqrt, cos, sin, fabs, asin, pi
from time import time

start_prog_time = time()
delts = [0.1, 0.01, 0.001]
int_val = 2 / (3 * sqrt(2))


def eval_int(x_cords, y_cords):
    res = 0
    last_x = next(x_cords)
    last_y = next(y_cords)
    for x, y in zip(x_cords, y_cords):
        dx = x - last_x
        dy = y - last_y
        last_x = x
        last_y = y
        res += (x ** 2 + y ** 2) * dx + (3 * x * y) * dy
    return res / 4


def check(x_, y_, _):
    return x_ ** 2 + y_ ** 2 <= 1 and x_ >= y_


def check_min(x_, y_, d_):
    return not ((x_ + d_ / 2) ** 2 + (y_ + d_ / 2) ** 2 > 1 or (x_ + d_ / 2) < (y_ + d_ / 2)) and \
        not ((x_ + d_ / 2) ** 2 + (y_ + -d_ / 2) ** 2 > 1 or (x_ + d_ / 2) < (y_ + -d_ / 2)) and \
        not ((x_ + -d_ / 2) ** 2 + (y_ + d_ / 2) ** 2 > 1 or (x_ + -d_ / 2) < (y_ + d_ / 2)) and \
        not ((x_ + -d_ / 2) ** 2 + (y_ + -d_ / 2) ** 2 > 1 or (x_ + -d_ / 2) < (y_ + -d_ / 2))


def check_max(x_, y_, d_):
    return ((x_ + d_ / 2) ** 2 + (y_ + d_ / 2) ** 2 <= 1 and (x_ + d_ / 2) >= (y_ + d_ / 2)) or \
        ((x_ + d_ / 2) ** 2 + (y_ + -d_ / 2) ** 2 <= 1 and (x_ + d_ / 2) >= (y_ + -d_ / 2)) or \
        ((x_ + -d_ / 2) ** 2 + (y_ + d_ / 2) ** 2 <= 1 and (x_ + -d_ / 2) >= (y_ + d_ / 2)) or \
        ((x_ + -d_ / 2) ** 2 + (y_ + -d_ / 2) ** 2 <= 1 and (x_ + -d_ / 2) >= (y_ + -d_ / 2))


def draw_ctr(delta):
    plt.figure(figsize=(10, 10))
    plt.axhline(0, color='black', linewidth=0.5)
    plt.axvline(0, color='black', linewidth=0.5)
    plt.title(f'partition for d = {delta}')
    plt.gca().set(xlim=(-1.5, 1.5), ylim=(-1.5, 1.5))
    plt.plot(x_dots, y_dots, marker='.', linestyle='-', markersize=3)

    plt.savefig(f'graph_partition_d_{delta}.png')


def draw_partition(type_, delta, all_rects, ind_, num):
    plt.figure(figsize=(10, 10))
    plt.axhline(0, color='black', linewidth=0.5)
    plt.axvline(0, color='black', linewidth=0.5)
    plt.title(f' {type_} for d = {delta}')
    plt.gca().set(xlim=(-1.5, 1.5), ylim=(-1.5, 1.5))
    plt.gca().add_collection(matplotlib.collections.PatchCollection(all_rects))
    plt.savefig(f'graph_{ind_ + 1}_d_{delta}_{num}.png')


def begin_x(c_y, delta):
    if c_y > -1 / sqrt(2) + 2 * delta:
        return c_y - 2 * delta
    else:
        return -c_y - sqrt(2) - 2 * delta


if __name__ == '__main__':

    # Делаем все для всех дельт
    for idx, d in enumerate(delts):
        start = time()
        x_dots = []
        y_dots = []

        # Считаем длину отрезка на ломаной и строим данныек точки
        x_val = -1 / sqrt(2)
        y_val = -1 / sqrt(2)
        while x_val ** 2 + y_val ** 2 < 1:
            x_dots.append(x_val)
            y_dots.append(y_val)
            x_val += d
            y_val += d
        x_dots.append(1 / sqrt(2))
        y_dots.append(1 / sqrt(2))

        int_sum = eval_int((i for i in x_dots), (i for i in y_dots))
        ind = len(x_dots)

        # Считаем длину полуокружности на ломаной и строим данные точки
        step_phi = 2 * asin(d / 2)
        start_phi = -3 * pi / 4
        while start_phi < pi / 4:
            x_dots.append(cos(start_phi))
            y_dots.append(sin(start_phi))
            start_phi += step_phi
        x_dots.append(cos(pi / 4))
        y_dots.append(sin(pi / 4))

        int_sum += eval_int((x_dots[i] for i in range(ind, len(x_dots))),
                            (y_dots[i] for i in range(ind, len(y_dots))))

        print(f'----d = {d}---- ')
        print(f'sum = {int_sum}')
        print(f'the error = {fabs(int_val - int_sum)}')
        print(f'time =  {time() - start}')
        print()

        start = time()
        int_sum = 0
        min_sum = 0
        max_sum = 0
        plot_squares_min = []
        plot_squares_max = []
        plot_squares = []

        # считаем интегральную сумму (в том числе min и max) пройодясь по графику слева направо и собирая нужные
        # квадраты
        cur_y = 1 / sqrt(2) + d
        while cur_y > -1 - d:
            cur_x = begin_x(cur_y, d)
            while cur_x < 1 + d:
                x = cur_x + d / 2
                y = cur_y - d / 2
                is_min = check_min(x, y, d)
                is_base = check(x, y, d)
                is_max = check_max(x, y, d)
                if not (is_min or is_base or is_max):
                    cur_x += d
                    continue

                rect = Rectangle((x - d / 2, y - d / 2), d, d)
                val = d ** 2 * y
                if is_min:
                    plot_squares_min.append(rect)
                    min_sum += val

                if is_base:
                    plot_squares.append(rect)
                    int_sum += val

                if is_max:
                    plot_squares_max.append(rect)
                    max_sum += val

                cur_x += d
            cur_y -= d

        int_sum *= -1
        max_sum *= -1
        min_sum *= -1

        print(f'int_sum = {int_sum}')
        print(f'the error = {fabs(int_val - int_sum)}')
        print(f'time = {time() - start}')
        print(f'max_sum - min_sum = {max_sum - min_sum}')
        print()

        # Рисуем все графики
        draw_ctr(d)
        draw_partition('sum', d, plot_squares, idx, 1)
        draw_partition('min sum', d, plot_squares_min, idx, 2)
        draw_partition('max sum', d, plot_squares_max, idx, 3)
        plt.close()

    print(f'time: {time() - start_prog_time}')
    print('end')
