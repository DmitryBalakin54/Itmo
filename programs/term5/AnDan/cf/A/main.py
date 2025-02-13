import math
import numpy as np


def predict_sequence():
    y = np.array([int(input()) for _ in range(168)], dtype=np.float64)
    t_values = np.arange(1, len(y) + 1, dtype=np.float64)
    n = len(y)
    best_m = np.array([12, 24, 168, 672], dtype=np.float64)

    def approximation(t, a, b):
        base = a[0]
        terms = a[1:] * np.sin((2 * np.pi * t[:, None]) / best_m + b)
        return base + terms.sum(axis=1)

    def error_function(a, b):
        predictions = approximation(t_values, a, b)
        return np.sqrt(np.mean((predictions - y) ** 2))

    def gradient_step(a, b, learning_rate):
        predictions = approximation(t_values, a, b)
        errors = predictions - y
        grad_a = np.zeros_like(a)
        grad_b = np.zeros_like(b)
        grad_a[0] = 2 * np.mean(errors)
        for i in range(1, len(best_m) + 1):
            sin_term = np.sin((2 * np.pi * t_values) / best_m[i - 1] + b[i - 1])
            cos_term = np.cos((2 * np.pi * t_values) / best_m[i - 1] + b[i - 1])
            grad_a[i] = 2 * np.mean(errors * sin_term)
            grad_b[i - 1] = 2 * np.mean(errors * a[i] * cos_term)
        a -= learning_rate * grad_a
        b -= learning_rate * grad_b
        return a, b

    np.random.seed(42)
    a = np.random.uniform(-0.5, 0.5, 5)
    b = np.random.uniform(-0.5, 0.5, 4)
    learning_rate = 0.04
    max_iterations = 10000
    tolerance = 1e-10

    for iteration in range(max_iterations):
        prev_error = error_function(a, b)
        a, b = gradient_step(a, b, learning_rate)
        current_error = error_function(a, b)

        if abs(prev_error - current_error) < tolerance:
            break

        if iteration % 1000 == 0:
            learning_rate *= 0.7

    future_t_values = np.arange(n + 1, n + 169, dtype=np.float64)
    future_values = approximation(future_t_values, a, b)
    future_values = np.round(future_values).astype(int)

    return future_values


result = predict_sequence()
print("\n".join(map(str, result)))