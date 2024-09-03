import numpy as np
import math

from scipy.optimize import minimize, approx_fprime

from newton import Newton
from newton_line_search import NewtonLineSearch
from newton_wolfie import NewtonWolfie


def run_method(class_method, f, x0, name):
    res = class_method.run(f, x0, name)

    print('-' * 4 + name + '-' * 4)
    print('Стартовая точка точка', x0)
    print('Минимум функции:', res['dot'])
    print('Значение функции в минимуме:', res['val_str'])

    if class_method.log_iter:
        print('iter= ', res['it'])

    print('-' * (8 + len(name)))


def solve(class_method):
    print('-' * 64 + class_method.__class__.__name__ + '-' * 64)
    run_method(class_method, lambda x: x[0] ** 2 + x[1] ** 2 + x[2] ** 2, np.array([2, -4, 12]),
               'x^2 + y^2 + z^2')

    run_method(class_method, lambda x: x[0] ** 2 + x[1] ** 2, np.array([-5, 5]),
               'x^2 + y^2')
    run_method(class_method, lambda x: (1 - x[0]) ** 2 + 2 * (x[1] - x[0] ** 2) ** 2, np.array([3, 10]),
               '(1 - x)^2 + 2 * (y - x^2)^2')

    run_method(class_method, lambda x: x[0] ** 4 - 3 * x[0] ** 3 + 2 * x[0] ** 2 - x[0], np.array([-1]),
               'x^4 - 3x^3 + 2x^2 -x')
    run_method(class_method, lambda x: math.sin(x[0]) ** 2 + 2 * math.cos(x[0]) ** 2, np.array([1]),
               'sin(x)^2 + 2 * cos(x)^2')
    # run_method(class_method, lambda x: -math.log10(1.0 / math.fabs(x[0])) * x[0], np.array([2]),
    #            '-log_10(1 / |x|) * x')
    run_method(class_method, lambda x: x[0] ** 2 - 2 * x[0] - 2, np.array([7]),
               'x^2 - 2x - 2')
    run_method(class_method, lambda x: x[0] ** 2 - 2 * x[0] - 2, np.array([-7]),
               'x^2 - 2x - 2')
    run_method(class_method, lambda x: x[0] ** 3 + x[0] ** 2 - 2 * x[0] - 2, np.array([3]),
               'x^3 + x^2 - 2x - 2')
    print('-' * (128 + len(class_method.__class__.__name__)), end='\n\n')


def run_newton_killer(nw, f, x0, name):
    nw.max_iter = 10000
    res = nw.run(f, x0, name)

    print('-' * 4 + name + '-' * 4)
    print('Стартовая точка точка', x0)
    print('Минимум функции:', res['dot'])
    print('Значение функции в минимуме:', res['val_str'])

    if nw.log_iter:
        print('iter= ', res['it'])

    res = minimize(f, x0, method='Newton-CG', jac=(lambda x: approx_fprime(x, f)))
    print(f'Newton-CG: x={res.x}, f(x)={f(res.x)}')

    print('-' * (8 + len(name)))


def kill_newton():
    nw = Newton(log_iter=True)
    print('-' * 64 + nw.__class__.__name__ + '-' * 64)

    run_newton_killer(nw, lambda x: (1 / 10 * x[0] ** 4 - 3 * x[0] ** 3 + 2 * x[0] ** 2 - x[0]), np.array([1]),
                      '1 / 10 * x^4 - 3x^3 + 2x^2 -x')  # не убивает, надо просто много итераций

    run_newton_killer(nw, lambda x: 1 / 4 * x[0] ** 4 - x[0] ** 2 + 2 * x, np.array([0]),
                      '1 / 4 x^4 - x^2 + 2x')  # в данной функции начиная в 0 шаг равен 1, потом в 1 шаг равен -1 и так длее
    # до бесконечности
    # либу по минимизации умные люди писали, они сначала градиентным спуском походу спускаются

    run_newton_killer(nw, lambda x: x[0] ** 4 - 3 * x[0] ** 3 + 2, np.array([-1]),
                      'x^4 - 3x^3 + 2')  # тут мой метод нньютона попадает на точку перегиба, где гессиан равен нулю
    # и закономерно падает

    run_newton_killer(nw, lambda x: x[0] ** 4 - 3 * x[0] ** 3 + 2, np.array([-0.00600002]),
                      'x^4 - 3x^3 + 2')  # а вот тут уже умирают все

    print('-' * (128 + len(nw.__class__.__name__)), end='\n\n')


def grad_plus_newton():
    f = lambda x: x[0] ** 4 - 3 * x[0] ** 3 + 2  # res = 2.25, к сожалению градиентный спуск либо прыгает куда не
    # надо, либо прыгает прямо в ответ, правда мой ньютон все же приближает ответ
    x0 = np.array([-1])
    x1 = minimize(f, x0, method='CG', jac=(lambda x: approx_fprime(x, f)), tol=1e-4).x
    nw = Newton(log_iter=True)

    print('-' * 64 + nw.__class__.__name__ + '-' * 64)
    run_newton_killer(nw, f, x1, 'x^4 -3x^3 + 2')
    print('-' * (128 + len(nw.__class__.__name__)), end='\n\n')


def different_points():
    nw = Newton(log_iter=True)
    line_nw = NewtonLineSearch(log_iter=True)

    f = lambda x: math.sin(x[0])
    x0 = np.array([math.pi * (1 + 1/ 4)])

    print(nw.run(f, x0, 'diff')['dot'])
    print(line_nw.run(f, x0, 'diff')['dot'])


if __name__ == '__main__':
    eps = 1e-6
    max_iter = 100000

    # solve(Newton(log_iter=True, log_plot=False, log_history=False, eps=eps, max_iter=max_iter))
    # solve(NewtonLineSearch(log_iter=True, log_plot=False, log_history=False, left=0.01, right=10.0, eps=eps,
    #                        max_iter=max_iter))
    # solve(NewtonWolfie(log_iter=True, log_plot=False, log_history=False, eps=eps, max_iter=max_iter, c1=0.01, c2=0.9,
    #                    alpha_init=1.5))
    #
    # kill_newton()
    # grad_plus_newton()

    different_points()
