import math
import time

import matplotlib.pyplot as plt

#p значения интегралов, полученные аналитически
leb = 40.0 / 99 + 1 + 2
leb_stil = math.pi + 18 + 12 / 990 + 8 / 5

# фуекция для нахождения индекса первой значащей цифры
def get_first_num_index(x):
    for ind, ch in enumerate(str(x)):
        if ch.isdigit() and ch != '0':
            return ind
    return -1


# функция для получения количества нулей в дробной части числа до первой значащей цифры
def zeros_after_dot(x):
    str_x = str(x)
    l_ind = str_x.index('.') if '.' in str_x else -1
    r_ind = get_first_num_index(x)
    return r_ind - l_ind - 1


# функция f
def f(x):
    if x == 0:
        return math.pi
    str_x = str(x)
    ind = get_first_num_index(x)
    val = int(str_x[ind])
    offset = 0
    if val > 4:
        offset = 1
    return float(str_x[:ind] + str(int(str_x[ind]) - offset))

# функции f_n
def f_n(n, x):
    if x > 3 or n < 1:
        raise ValueError(f'illegal arguments ' + f'x = {x}' if x > 3 else f'n = {n}')
    if x == 0:
        return math.pi
    if 1 <= x < 2:
        return 1
    if 2 <= x < 3:
        return 2
    if x == 3:
        return 3
    if 0 <= zeros_after_dot(x) <= n:
        return f(x)
    return 0

# F
def F(x):
    return 3 * x - math.floor(-2 * x)

# функция которая рисует графики f_n
def draw_f_n(l, r, grid=True, zoom=False):
    max_n = 3
    eps = 0.01

    for frame in range(max_n):
        fig, ax = plt.subplots()
        ax.clear()
        if not zoom:
            ax.set_xlim(l, r)
            ax.set_ylim(l - eps, r)
        else:
            offset = (r - l) / 10 ** max_n * int('9' * max_n)
            ax.set_xlim(l, r - offset)
            ax.set_ylim(l - offset / 10000, r - offset)

        ax.set_xlabel('x')
        ax.set_ylabel('f(x)')
        ax.set_title(f'График функции f_n(x) для n={frame + 1}')
        ax.grid(grid)
        x_values = [l + i / 10 ** (frame + 3) for i in range(int((r - l) * 10 ** (frame + 1) + 1))]
        x_values.extend([l + i / 10 ** (frame + 1) for i in range(int((r - l) * 10 ** (frame + 1) + 1))])
        y_values = [f_n(frame + 1, x) for x in x_values]
        ax.scatter(x_values, y_values, s=10)
        name = f'frame_{frame + 1}' + ('_z' if zoom else '') + '.png'

        fig.savefig(name)
        plt.close(fig)

# функция для подсчета интеграла Лебега
def leb_int(n):
    res = 0

    for i in range(1, n):
        res += (40 / 10 ** i) * (1.0 / 10 ** i)

    for i in range(1, 3):
        res += i

    return res

# функция для подсчета интеграла Лебега-Стилтьеса
def leb_stil_int(n):
    res = 0
    for i in range(2, n):
        res += 120 * (1 / 10 ** i) * (1.0 / 10 ** i)

    return res + 18 + math.pi + 8 / 5

# main с выводом всего что нужно и с подсчетом времени
if __name__ == '__main__':
    all_time = time.time()

    draw_f_n(0, 3, True, True)
    draw_f_n(0, 3, True, False)

    for n in [1, 3, 5, 7, 10, 100]:
        leb_time = time.time()
        res = leb_int(n)
        print(f'интеграл Лебега для  n={n}: {res:.20f}, разница с точным результатом: {math.fabs(res - leb):.20f}, time={1000 * (time.time() - leb_time)}')

    print()

    for n in [1, 3, 5, 7, 10, 100]:
        leb_ctil_time = time.time()
        res = leb_stil_int(n)
        print(f'интеграл Лебега-Стильтеса для  n={n}: {res:.20f}, разница с точным результатом: {math.fabs(res - leb_stil):.20f}, time={1000 * (time.time() - leb_ctil_time)}')
    print(f'\ntotal time={1000 * (time.time() - all_time)}')
