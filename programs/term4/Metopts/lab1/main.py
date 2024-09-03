import numpy as np

import grad_learning_rate as grad
import grad_dichotomy as dech
import grad_golden_ratio as gold
import Nelder_Mead as nM

grad.step = 0.02
grad.eps = 0.00001
grad.max_iter = 10000
grad.levels = 10

dech.eps = 0.00001
dech.max_iter = 10000
dech.left = 0.0
dech.right = 0.5
dech.levels = 10

gold.eps = 0.00001
gold.max_iter = 10000
gold.left = 0.0
gold.right = 0.5
gold.levels = 10

nM.eps = 0.00001
nM.levels = 10

def run_f(gr, f, arg0, arg1, name, log, log_plt, log_history, plt_scale):

    gr.log_iter = log
    gr.log_plot = log_plt
    gr.log_history = log_history
    gr.plot_scale = plt_scale
    gr.name = name

    res = gr.run(f, arg0, arg1)
    res1 = [format(i, ".5f") for i in res]
    # res = [round(i, 5) for i in res]
    print(f'f = {name}, X = {res1}, res = {format(f(*res), ".5f")}', end='')
    if log:
        print(f', iters = {gr.it}', end='')
    print()


def run_grad(gr, arg0, arg1, log=False, log_plt=False, log_history=False, plt_scale=1):
    print('-' * 3 + f'{gr.__name__} x = {arg0} y = {arg1}' + '-' * 3)

    run_f(gr, lambda x, y: 100 * x ** 2 + y ** 2, arg0, arg1, 'x^2 + y^2', log, log_plt, log_history, plt_scale)
    # run_f(gr, lambda x, y: x ** 2 - 2 * x + y ** 2 + 1, arg0, arg1,'x^2 - 2x + y^2 + 1', log, log_plt, log_history, plt_scale)
    run_f(gr, lambda x, y: 10000 * x ** 2 - 200 * x * y + y ** 2 + 1, arg0, arg1, 'x^2 - 2xy + y^2 + 1', log, log_plt, log_history, plt_scale)
    # run_f(gr, lambda x, y: np.sin(x) ** 2 + np.cos(y) ** 2, arg0, arg1, 'sin(x)^2 + cos(x)^2', log, log_plt, log_history, plt_scale)


run_grad(grad, -100, 50, True, log_history=True)
run_grad(dech, -100, 50, True)
run_grad(gold, -100, 50, True)
run_grad(nM, -100, 50, True)

print(end="\n\n\n")

run_grad(grad, 4, -3, True)
run_grad(dech, 4, -3, True)
run_grad(gold, 4, -3, True)
run_grad(nM, 4, -3, True)
