import math
import numpy as np
from matplotlib.ticker import LinearLocator
from scipy.optimize import approx_fprime
import matplotlib.pyplot as plt


class Newton:
    def __init__(self, eps=1e-6, max_iter=100, log_iter=False, log_plot=False, log_history=False, levels=10):
        self.levels = levels
        self.log_history = log_history
        self.log_plot = log_plot
        self.log_iter = log_iter
        self.max_iter = max_iter
        self.eps = eps
        self.counter = 0
        self.history = []
        self.it = 0

    def set_default(self):
        self.eps = 1e-6
        self.max_iter = 100
        self.log_iter = False
        self.it = 0
        self.log_plot = False
        self.log_history = False
        self.levels = 10
        self.history = []

    def step(self, f, epsilon, x0, grd_dot_inv_hess):
        return 1

    def newton_method(self, f, x0):
        cur_x = x0
        for _ in range(self.max_iter):
            if self.log_iter:
                self.it += 1

            if self.log_plot or self.log_history:
                self.history.append(cur_x)

            grad = approx_fprime(cur_x, f, epsilon=1e-6)
            hessian = approx_fprime(cur_x, lambda x: approx_fprime(x, f, epsilon=1e-6), epsilon=1e-6)

            if len(x0) + 1 > 2:
                grd_dot_inv_hess = np.dot(np.linalg.linalg.inv(hessian), grad)
            else:
                grd_dot_inv_hess = grad / hessian

            new_x = cur_x - self.step(f, self.eps, cur_x, grd_dot_inv_hess) * grd_dot_inv_hess

            if np.linalg.norm(new_x - cur_x) < self.eps:
                if self.log_plot or self.log_history:
                    self.history.append(new_x)

                return new_x

            cur_x = new_x

        if self.log_plot or self.log_history:
            self.history.append(cur_x)

        return cur_x

    def get_abs_dot(self, d1, d2, offset):
        dot = np.array([0.0 for _ in d1])
        for i in range(len(d1)):
            dot[i] = d1[i] \
                if math.fabs(d1[i] - offset[i]) > math.fabs(d2[i] - offset[i]) \
                else d2[i]

        return dot

    def get_max_dot(self, center, start):
        res = start
        if not self.log_history:
            return np.array(res)

        res = self.get_abs_dot(res, res, center)
        for dot in self.history:
            res = self.get_abs_dot(res, dot, center)

        return res

    def get_X_Y_Z(self, start, finish, f):
        xl = finish[0] - math.fabs(finish[0] - start[0])
        xr = finish[0] + math.fabs(finish[0] - start[0])
        yl = finish[1] - math.fabs(finish[1] - start[1])
        yr = finish[1] + math.fabs(finish[1] - start[1])

        offset = math.fabs(xr - xl) / 10

        xl = (xr + xl) / 2 - math.fabs(xr - xl) / 2 - offset
        xr = (xr + xl) / 2 + math.fabs(xr - xl) / 2 + offset
        yl = (yr + yl) / 2 - math.fabs(yr - yl) / 2 - offset
        yr = (yr + yl) / 2 + math.fabs(yr - yl) / 2 + offset

        X = np.arange(xl, xr, (xr - xl) / 100.0)
        Y = np.arange(yl, yr, (yr - yl) / 100.0)
        X, Y = np.meshgrid(X, Y)
        Z = f(np.array([X, Y]))

        return X, Y, Z

    def get_X_Y(self, start, finish, f):
        xl = min(start, finish)
        xr = max(start, finish)

        offset = math.fabs(xr - xl)

        xl = xl - offset
        xr = xr + offset

        X = np.linspace(xl, xr, 1000)
        Y = np.zeros_like(X)
        for i, x in enumerate(X):
            Y[i] = f(x)

        return X, Y

    def make_new_name(self, name):
        return name.replace('*', '').replace('/', 'div').replace('|', 'm')

    def draw_2D(self, start, finish, f, name):
        X, Y = self.get_X_Y(start, finish, f)

        plt.plot(X, Y, color='b')
        plt.title(name)
        plt.xlabel('x')
        plt.ylabel('y')
        new_name = self.make_new_name(name)
        plt.savefig(f'figs/{__name__}_{new_name}_{self.counter}.png', dpi=400)
        plt.close('all')
        self.counter += 1

    def draw_3D(self, start, finish, f, name):
        X, Y, Z = self.get_X_Y_Z(start, finish, f)
        fig, ax = plt.subplots(subplot_kw={"projection": "3d"})
        ax.zaxis.set_major_locator(LinearLocator(10))
        ax.zaxis.set_major_formatter('{x:.02f}')

        ax.plot_surface(X, Y, Z, alpha=0.4, color='b')
        new_name = self.make_new_name(name)
        plt.savefig(f'figs/{__name__}_{new_name}_{self.counter}.png', dpi=400)
        plt.close('all')
        self.counter += 1

    def draw_way_2D(self, start, finish, f, name):
        X, Y = self.get_X_Y(start, finish, f)

        plt.plot(X, Y, color='b')
        plt.scatter(self.history, [f(i) for i in self.history])
        plt.title(name)
        f_prime = (lambda x: approx_fprime(x, f, epsilon=self.eps))
        X, Y = self.get_X_Y(start, finish, f_prime)
        plt.plot(X, Y, color='r')
        plt.scatter(self.history, [f_prime(i) for i in self.history])
        plt.xlabel('x')
        plt.ylabel('y')
        new_name = self.make_new_name(name)
        plt.savefig(f'figs/{__name__}_levels_{new_name}_{self.counter}.png', dpi=400)
        plt.close('all')
        self.counter += 1

    def draw_way_3D(self, start, finish, f, name):
        X, Y, Z = self.get_X_Y_Z(start, finish, f)
        plt.contour(X, Y, Z, levels=self.levels)
        plt.scatter([i[0] for i in self.history], [i[1] for i in self.history])

        plt.grid(True)
        new_name = self.make_new_name(name)
        plt.savefig(f'figs/{__name__}_levels_{new_name}_{self.counter}.png')
        self.counter += 1
        plt.close('all')

    def run(self, f, x0, name=''):
        np.set_printoptions(suppress=True)

        dim = len(x0) + 1

        x_min = self.newton_method(f, x0)
        f_x_min = f(x_min)
        f_x_min_str = np.format_float_positional(f_x_min)

        if self.log_plot and dim == 2:
            self.draw_2D(self.get_max_dot(x_min, x0), x_min, f, name)
        if self.log_plot and dim == 3:
            self.draw_3D(self.get_max_dot(x_min, x0), x_min, f, name)
        if self.log_history and dim == 2:
            self.draw_way_2D(self.get_max_dot(x_min, x0), x_min, f, name)
        if self.log_history and dim == 3:
            self.draw_way_3D(self.get_max_dot(x_min, x0), x_min, f, name)

        self.history = []
        new_it = self.it
        self.it = 0
        return {'dot': x_min, 'val': f_x_min, 'val_str': f_x_min_str, 'it': new_it}
