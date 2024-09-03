import math
import numpy as np
import scipy.optimize as opt
import matplotlib.pyplot as plt
from matplotlib.ticker import LinearLocator

it = 0
eps = 0.000001
max_iter = 10000
log_iter = False
log_plot = False
log_history = True
plot_scale = 1
history = []
counter = 0
name = ''
levels = 10

def ob(f, X):
    if log_history:
        history.append(X)
    return f(*X)


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
    global counter, history, it, name
    history = []
    dot = np.array(start_dot)
    res = opt.minimize(lambda X: ob(f, X), dot, method='Nelder-Mead', options={'maxiter': max_iter}, tol=0.000001)
    it = res['nit']
    # opt.s
    # print(res)

    dot = get_max_dot(res['x'], dot)
    if log_history or log_plot:
        xl = res['x'][0] - math.fabs(res['x'][0] - dot[0])
        xr = res['x'][0] + math.fabs(res['x'][0] - dot[0])
        yl = res['x'][1] - math.fabs(res['x'][1] - dot[1])
        yr = res['x'][1] + math.fabs(res['x'][1] - dot[1])

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
        plt.savefig(f'figs/Nelder_Mead_{name}_{counter}.png', dpi=400)
        plt.close('all')
        counter += 1

    if log_history:
        plt.contour(X, Y, Z, levels=levels)
        plt.scatter([i[0] for i in history], [i[1] for i in history])
        plt.grid(True)
        plt.savefig(f'figs/Nelder_Mead_levels_{name}_{counter}.png')
        counter += 1
        plt.close('all')
    name = ''

    return res['x']


if __name__ == '__main__':
    print(run(lambda x, y: x ** 2 - 2 * x * y + y ** 2 + 1, -100, 50))
    print(run(lambda x, y: x ** 2 - 2 * x * y + y ** 2 + 1, 4, -3))
    # print(history)
