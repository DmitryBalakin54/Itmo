import math

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.ticker import LinearLocator

step = 0.1
eps = 0.000001
max_iter = 10000

log_iter = False
it = 0

log_plot = False
log_history = False
plot_scale = 1
history = []
counter = 0
name = ''
levels = 10

grad_func = (
    lambda f: lambda *args: np.array([(f(*(args + offset)) - f(*args)) / eps for offset in np.eye(len(args)) * eps]))

next_dot_func = (lambda f: lambda args: np.array(args) - step * grad_func(f)(*args))

calc_stop_func = (lambda dot_1, dot_2: math.sqrt(sum((dot_1[i] - dot_2[i]) ** 2 for i in range(len(dot_1)))))

stop_criteria_func = (lambda dot_1, dot_2: calc_stop_func(dot_1, dot_2) < eps)


def gradient_descent(f, *start_dot):
    global it, history

    if log_iter:
        it = 1

    next_dot = next_dot_func(f)
    last_dot = np.array(start_dot)

    if log_history:
        history.append(last_dot)

    dot = next_dot(last_dot)

    if log_history:
        history.append(dot)

    for _ in range(max_iter):
        if log_iter:
            it += 1

        last_dot, dot = dot, next_dot(dot)

        if log_history:
            history.append(dot)

        if stop_criteria_func(last_dot, dot):
            break

    return dot


def get_abs_dot(d1, d2, offset):
    dot = np.array([0.0 for _ in d1])
    for i in range(len(d1)):
        dot[i] = d1[i] \
            if math.fabs(d1[i] - offset[i]) > math.fabs(d2[i] - offset[i]) \
            else d2[i]

    return dot


def get_max_dot(center, start):
    res = start
    if not log_history:
        return np.array(res)

    res = get_abs_dot(res, res, center)
    for dot in history:
        res = get_abs_dot(res, dot, center)

    return res


def run(f, *start_dot):
    global counter, name, history, it
    history = []
    res = gradient_descent(f, *start_dot)

    dot = np.array(start_dot)
    dot = get_max_dot(res, dot)
    if log_history or log_plot:
        xl = res[0] - math.fabs(res[0] - dot[0])
        xr = res[0] + math.fabs(res[0] - dot[0])
        yl = res[1] - math.fabs(res[1] - dot[1])
        yr = res[1] + math.fabs(res[1] - dot[1])

        offset = math.fabs(xr - xl) / 10

        xl = (xr + xl) / 2 - plot_scale * math.fabs(xr - xl) / 2 - offset
        xr = (xr + xl) / 2 + plot_scale * math.fabs(xr - xl) / 2 + offset
        yl = (yr + yl) / 2 - plot_scale * math.fabs(yr - yl) / 2 - offset
        yr = (yr + yl) / 2 + plot_scale * math.fabs(yr - yl) / 2 + offset

        X = np.arange(xl, xr, (xr - xl) / 100.0)
        Y = np.arange(yl, yr, (yr - yl) / 100.0)
        X, Y = np.meshgrid(X, Y)
        Z = f(X, Y)

    if log_plot:
        fig, ax = plt.subplots(subplot_kw={"projection": "3d"})
        ax.zaxis.set_major_locator(LinearLocator(10))
        ax.zaxis.set_major_formatter('{x:.02f}')

        ax.plot_surface(X, Y, Z, alpha=0.4, color='b')
        plt.savefig(f'figs/grad_learning_rate_{name}_{counter}.png', dpi=400)
        plt.close('all')
        counter += 1

    if log_history:
        plt.contour(X, Y, Z, levels=levels)
        plt.scatter([i[0] for i in history], [i[1] for i in history])

        plt.grid(True)
        plt.savefig(f'figs/grad_learning_rate_levels_{name}_{counter}.png')
        counter += 1
        plt.close('all')
    name = ''

    return res


if __name__ == '__main__':
    print(run(lambda x, y: x ** 2 + y ** 2, 10, -10))
    print(run(lambda x, y: x ** 2 - 2 * x + y ** 2 + 1, -30, 40))
