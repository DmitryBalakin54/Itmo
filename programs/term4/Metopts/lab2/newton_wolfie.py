import numpy as np
from scipy.optimize import approx_fprime
from newton import Newton


class NewtonWolfie(Newton):
    def __init__(self, eps=1e-6, max_iter=100, log_iter=False, log_plot=False, log_history=False, levels=10,
                 c1=1e-5, c2=0.99, alpha_init=1.5, max_wolfie_iter=100):
        super().__init__(eps, max_iter, log_iter, log_plot, log_history, levels)
        self.c1 = c1
        self.c2 = c2
        self.alpha_init = alpha_init
        self.max_wolfie_iter = max_wolfie_iter

    def step(self, f, epsilon, x0, grd_dot_inv_hess):
        alpha = self.alpha_init
        f_prime = (lambda x: approx_fprime(x, f, epsilon=epsilon))
        for _ in range(self.max_wolfie_iter):
            if (f(x0 - alpha * grd_dot_inv_hess) > f(x0) - self.c1 * alpha *
                    (np.dot(f_prime(x0), grd_dot_inv_hess) if len(x0) > 1 else f_prime(x0) * grd_dot_inv_hess)):
                alpha *= 0.5
            else:
                if (np.dot(f_prime(x0 - alpha * grd_dot_inv_hess), grd_dot_inv_hess) < -self.c2 *
                        (np.dot(f_prime(x0), grd_dot_inv_hess) if len(x0) > 1 else f_prime(x0) * grd_dot_inv_hess)):
                    alpha *= 2.0
                else:
                    return alpha

        return alpha
