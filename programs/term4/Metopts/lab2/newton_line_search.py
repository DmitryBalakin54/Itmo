import math

from newton import Newton


class NewtonLineSearch(Newton):
    def __init__(self, eps=1e-6, max_iter=100, log_iter=False, log_plot=False, log_history=False, levels=10, left=0.01,
                 right=10):
        super().__init__(eps, max_iter, log_iter, log_plot, log_history, levels)
        self.left = left
        self.right = right

    def step(self, f, epsilon, x0, grd_dot_inv_hess):
        delta = epsilon / 2
        res = 1
        l = self.left
        r = self.right

        for _ in range(self.max_iter):
            x2 = l + (0.5 + delta) * (r - l)
            x1 = l + (0.5 - delta) * (r - l)

            f_x1 = f(x0 - x1 * grd_dot_inv_hess)
            f_x2 = f(x0 - x2 * grd_dot_inv_hess)

            if f_x1 < f_x2:
                r = x2
            elif f_x2 < f_x1:
                l = x1
            else:
                res = math.fabs(x1 + x2) / 2
                break

            res = math.fabs(r + l) / 2
            if math.fabs(r - l) < epsilon:
                break

        return res
