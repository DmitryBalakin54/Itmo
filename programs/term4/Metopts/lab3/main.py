import numpy as np
import matplotlib.pyplot as plt
from sklearn.preprocessing import PolynomialFeatures


def predict(X, theta):
    return X.dot(theta)


def compute_mse(y_true, y_pred):
    return np.mean((y_true - y_pred) ** 2)


def sgd(X, y, batch_size, learning_rate_schedule, name, n_epochs=50, momentum=0.9, epsilon=1e-6, regularization=0, lambda1=0.01, lambda2=0.01):
    m, n = X.shape
    theta = np.random.randn(n, 1)
    learning_rate = learning_rate_schedule(0)
    history = []
    error_velocity = 0

    for epoch in range(n_epochs // (m // batch_size)):
        shuffled_indices = np.random.permutation(m)
        X_shuffled = X[shuffled_indices]
        y_shuffled = y[shuffled_indices]

        for i in range(0, m, batch_size):
            X_batch = X_shuffled[i:i + batch_size]
            y_batch = y_shuffled[i:i + batch_size]

            gradients = 2 / batch_size * X_batch.T.dot(predict(X_batch, theta) - y_batch)

            if regularization == 1:
                gradients += lambda1 * np.sign(theta)
            elif regularization == 2:
                gradients += lambda2 * theta
            elif regularization == 3:
                gradients += lambda1 * np.sign(theta) + lambda2 * theta

            theta -= learning_rate * gradients

            learning_rate = learning_rate_schedule((epoch - 1) // (m // batch_size + i))

            y_pred = predict(X, theta)
            mse = compute_mse(y, y_pred)
            error_velocity = momentum * error_velocity + (1 - momentum) * mse
            history.append(mse)

            print(f'Epoch {epoch}, MSE: {mse}, {name}: {learning_rate}')

            if error_velocity < epsilon:
                print(f'Training stopped at epoch {epoch} due to error {error_velocity} being less than epsilon {epsilon}')
                break

    return theta, history


def exponential_decay(initial_lr, decay_rate):
    return lambda t: initial_lr * np.exp(-decay_rate * t)


def step_decay(initial_lr, drop, epochs_drop):
    return lambda epoch: initial_lr * drop ** (epoch // epochs_drop)


def const_step(const):
    return lambda _: const


def generate_linear_data_with_noise(n_samples=1000, n_features=1, noise_level=1.0):
    X = 2 * np.random.rand(n_samples, n_features)
    y = 1 + X.dot(np.ones((n_features, 1))) + noise_level * np.random.randn(n_samples, 1)
    return X, y


def generate_linear_data(n_samples=1000, n_features=1):
    X = 2 * np.random.rand(n_samples, n_features)
    y = 1 + X.dot(np.ones((n_features, 1)))
    return X, y


def generate_polynomial_data(n_samples=1000, degree=2, noise_level=0.1):
    X = 2 * (np.random.rand(n_samples, 1) - 0.5)
    y = 5 + np.power(X, degree) + noise_level * (np.random.randn(n_samples, 1) - 0.5)
    return X, y


def generate_exponential_data(n_samples=1000, noise_level=0.1):
    X = 2 * np.random.rand(n_samples, 1)
    y = 1 + np.exp(X) + noise_level * np.random.randn(n_samples, 1)
    return X, y


def generate_linear_data_with_uniform_noise(n_samples=1000, n_features=1, noise_level=0.1):
    X = 2 * np.random.rand(n_samples, n_features)
    y = 1 + X.dot(np.ones((n_features, 1)))
    uniform_noise = np.random.uniform(-noise_level, noise_level, (n_samples, 1))
    y += uniform_noise
    return X, y


def add_polynomial_features(X, degree):
    poly = PolynomialFeatures(degree)
    return poly.fit_transform(X)


batch_sizes = [4, 10, 50, 100, 500, 1000]
initial_lr = 0.01
decay_rate = 0.01
drop = 0.99
epochs_drop = 500
step = 0.01
epochs = 10000
samples = 10
momentum = 0.1
regularization = 3
l1 = 0.0
l2 = 0.0
degree = 5


# X, y = generate_linear_data_with_noise(samples, 1, 0.1)
# X, y = generate_linear_data(samples, 1)
X, y = generate_polynomial_data(samples, degree, 0)
# X, y = generate_exponential_data(samples)
# X, y = generate_linear_data_with_uniform_noise(samples, 1, 0.8)

X = np.array([[-1.5], [-0.5], [1], [2]])
y = np.array([[-0.8], [2], [-1], [1]])

X_poly = add_polynomial_features(X, degree)
res = []
for batch_size in batch_sizes[:1]:
    print(f"\nBatch Size: {batch_size}")

    theta_ladder, history_ladder = sgd(X_poly, y, batch_size, step_decay(initial_lr / (degree * 2), drop, epochs_drop), "Drop step",
                                       epochs, momentum, epsilon=1e-6, regularization=regularization, lambda1=l1, lambda2=l2)
    theta_step, history_step = sgd(X_poly, y, batch_size, const_step(step / (degree * 2)), 'Const step', epochs, momentum,
                                   epsilon=1e-6, regularization=regularization, lambda1=l1, lambda2=l2)

    print(f"Ladder ans = {theta_ladder}")
    print(f"Const step ans = {theta_step}")

    plt.figure(figsize=(14, 7))

    # Generate dense set of points for smooth plotting
    X_dense = np.linspace(X.min(), X.max(), 100).reshape(-1, 1)
    X_dense_poly = add_polynomial_features(X_dense, degree)

    plt.subplot(1, 2, 1)
    plt.scatter(X, y, color='blue', marker='o', label='Data points')
    y_pred_ladder = predict(X_dense_poly, theta_ladder)
    plt.plot(X_dense, y_pred_ladder, color='red', linewidth=2, label='Model prediction (Ladder)')
    plt.title(f'Batch Size {batch_size} - Ladder')
    plt.xlabel('Feature')
    plt.ylabel('Target')
    plt.legend()

    plt.subplot(1, 2, 2)
    plt.scatter(X, y, color='blue', marker='o', label='Data points')
    y_pred_step = predict(X_dense_poly, theta_step)
    plt.plot(X_dense, y_pred_step, color='green', linewidth=2, label='Model prediction (Const step)')
    plt.title(f'Batch Size {batch_size} - Const step')
    plt.xlabel('Feature')
    plt.ylabel('Target')
    plt.legend()

    plt.show()

    res.append((history_ladder, f'Batch Size {batch_size} - Ladder'))
    res.append((history_step, f'Batch Size {batch_size} - Const step'))

for dots, labl in res:
    plt.plot(dots, label=labl)
plt.xlabel('Epoch')
plt.ylabel('MSE')
plt.legend()
plt.title('SGD with Different Batch Sizes and Learning Rate Schedules')
plt.show()
