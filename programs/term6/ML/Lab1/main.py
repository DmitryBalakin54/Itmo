import matplotlib.pyplot as plt
import numpy as np
from sklearn.datasets import make_classification
from sklearn.metrics import accuracy_score
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler

X, y = make_classification(
    n_samples=10000,
    n_features=20,
    n_informative=15,
    n_redundant=5,
    n_classes=3,
)

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.8)

scaler = StandardScaler()
X_train = scaler.fit_transform(X_train)
X_test = scaler.transform(X_test)


def one_hot_encode(y, num_classes):
    return np.eye(num_classes)[y]


y_train_onehot = one_hot_encode(y_train, 3)
y_test_onehot = one_hot_encode(y_test, 3)


class LinearTransform:
    def __init__(self, input_dim, output_dim, activation='relu'):
        self.grad_bias = None
        self.grad_weights = None
        self.a = None
        self.z = None
        self.x = None
        self.weights = np.random.randn(output_dim, input_dim) * np.sqrt(
            2.0 / (input_dim + output_dim))
        self.bias = np.zeros(output_dim)
        self.activation = activation

    def forward(self, x):
        self.x = x
        self.z = np.dot(x, self.weights.T) + self.bias
        if self.activation == 'relu':
            self.a = np.maximum(0, self.z)
        elif self.activation == 'tanh':
            self.a = np.tanh(self.z)
        elif self.activation == 'identity':
            self.a = self.z
        return self.a

    def backward(self, grad_output):
        grad_z = 0
        if self.activation == 'relu':
            grad_z = grad_output * (self.z > 0)
        elif self.activation == 'tanh':
            grad_z = grad_output * (1 - np.tanh(self.z) ** 2)
        elif self.activation == 'identity':
            grad_z = grad_output

        self.grad_weights = np.dot(grad_z.T, self.x)
        self.grad_bias = np.sum(grad_z, axis=0)
        grad_input = np.dot(grad_z, self.weights)
        return grad_input

    def update_parameters(self, lr):
        self.weights -= lr * self.grad_weights
        self.bias -= lr * self.grad_bias


class RBFTransform:
    def __init__(self, input_dim, output_dim, gamma=1.0):
        self.grad_centers = None
        self.a = None
        self.distances = None
        self.x = None
        self.centers = np.random.randn(output_dim, input_dim) * np.sqrt(
            2.0 / (input_dim + output_dim))
        self.gamma = gamma

    def forward(self, x):
        self.x = x
        self.distances = np.sum((x[:, np.newaxis, :] - self.centers[np.newaxis, :, :]) ** 2, axis=2)
        self.a = np.exp(-self.gamma * self.distances)
        return self.a

    def backward(self, grad_output):
        grad_distances = -self.gamma * grad_output * self.a
        grad_centers = -2 * self.gamma * np.sum(
            (self.x[:, np.newaxis, :] - self.centers[np.newaxis, :, :]) * grad_distances[:, :, np.newaxis], axis=0)
        grad_input = 2 * self.gamma * np.sum(
            (self.x[:, np.newaxis, :] - self.centers[np.newaxis, :, :]) * grad_distances[:, :, np.newaxis], axis=1)
        self.grad_centers = grad_centers
        return grad_input

    def update_parameters(self, lr):
        self.centers -= lr * self.grad_centers


def soft_arg_max_cross_entropy(y_pred, y_true):
    m = y_true.shape[0]
    exp_y_pred = np.exp(y_pred - np.max(y_pred, axis=1, keepdims=True))
    softmax_probs = exp_y_pred / np.sum(exp_y_pred, axis=1, keepdims=True)
    log_probs = -np.log(softmax_probs[range(m), y_true.argmax(axis=1)])
    loss = np.sum(log_probs) / m
    return loss, softmax_probs


def soft_arg_max_cross_entropy_grad(y_pred, y_true):
    m = y_true.shape[0]
    exp_y_pred = np.exp(y_pred - np.max(y_pred, axis=1, keepdims=True))
    softmax_probs = exp_y_pred / np.sum(exp_y_pred, axis=1, keepdims=True)
    grad = softmax_probs.copy()
    grad[range(m), y_true.argmax(axis=1)] -= 1
    grad /= m
    return grad


def clip_gradients(model, clip_value=0.5):
    for layer in model:
        if hasattr(layer, 'grad_weights'):
            layer.grad_weights = np.clip(layer.grad_weights, -clip_value, clip_value)
        if hasattr(layer, 'grad_bias'):
            layer.grad_bias = np.clip(layer.grad_bias, -clip_value, clip_value)


def train_model(model, X_train, X_test, y_test, epochs=400, lr=0.3, l2_lambda=0.01):
    train_losses = []
    test_accuracies = []

    for epoch in range(epochs):
        outputs = X_train
        for layer in model:
            outputs = layer.forward(outputs)

        loss, _ = soft_arg_max_cross_entropy(outputs, y_train_onehot)
        l2_loss = sum(np.sum(layer.weights ** 2) for layer in model if hasattr(layer, 'weights'))
        loss += l2_lambda * l2_loss
        train_losses.append(loss)

        grad = soft_arg_max_cross_entropy_grad(outputs, y_train_onehot)
        for layer in reversed(model):
            grad = layer.backward(grad)

        clip_gradients(model, clip_value=1.0)

        for layer in model:
            if hasattr(layer, 'weights'):
                layer.grad_weights += 2 * l2_lambda * layer.weights
            layer.update_parameters(lr)

        test_outputs = X_test
        for layer in model:
            test_outputs = layer.forward(test_outputs)

        exp_test_outputs = np.exp(test_outputs - np.max(test_outputs, axis=1, keepdims=True))
        softmax_test_probs = exp_test_outputs / np.sum(exp_test_outputs, axis=1, keepdims=True)
        test_pred = np.argmax(softmax_test_probs, axis=1)
        accuracy = accuracy_score(y_test, test_pred)
        test_accuracies.append(accuracy)

        if epoch % 50 == 0 and epoch != 0:
            lr *= 0.9

    return train_losses, test_accuracies


def plot_results(results, title):
    plt.figure(figsize=(12, 5))

    plt.subplot(1, 2, 1)
    for i, (train_losses, _) in enumerate(results):
        plt.plot(train_losses, label=f'Model {i + 1}')
    plt.title(f'{title} - Training Loss')
    plt.xlabel('Epoch')
    plt.ylabel('Loss')
    plt.legend()

    plt.subplot(1, 2, 2)
    for i, (_, test_accuracies) in enumerate(results):
        plt.plot(test_accuracies, label=f'Model {i + 1}')
    plt.title(f'{title} - Test Accuracy')
    plt.xlabel('Epoch')
    plt.ylabel('Accuracy')
    plt.legend()

    plt.savefig(title + '.png')


def train_linear_models():
    models = [
        [LinearTransform(input_dim=20, output_dim=15, activation='relu'),
         LinearTransform(input_dim=15, output_dim=3, activation='identity')],
        [LinearTransform(input_dim=20, output_dim=15, activation='relu'),
         LinearTransform(input_dim=15, output_dim=10, activation='tanh'),
         LinearTransform(input_dim=10, output_dim=3, activation='identity')],
        [LinearTransform(input_dim=20, output_dim=13, activation='relu'),
         LinearTransform(input_dim=13, output_dim=17, activation='tanh'),
         LinearTransform(input_dim=17, output_dim=10, activation='relu'),
         LinearTransform(input_dim=10, output_dim=3, activation='identity')]
    ]

    results = []
    for model in models:
        train_losses, test_accuracies = train_model(model, X_train, X_test, y_test)
        results.append((train_losses, test_accuracies))

    return results


def train_rbf_models():
    models = [
        [LinearTransform(input_dim=20, output_dim=15, activation='relu'),
         RBFTransform(input_dim=15, output_dim=10),
         LinearTransform(input_dim=10, output_dim=3, activation='identity')],
        [LinearTransform(input_dim=20, output_dim=10, activation='relu'),
         RBFTransform(input_dim=10, output_dim=6),
         RBFTransform(input_dim=6, output_dim=10),
         LinearTransform(input_dim=10, output_dim=3, activation='identity')],
        [LinearTransform(input_dim=20, output_dim=10, activation='relu'),
         RBFTransform(input_dim=10, output_dim=6),
         RBFTransform(input_dim=6, output_dim=10),
         RBFTransform(input_dim=10, output_dim=10),
         LinearTransform(input_dim=10, output_dim=3, activation='identity')]
    ]


    results = []
    for model in models:
        train_losses, test_accuracies = train_model(model, X_train, X_test, y_test)
        results.append((train_losses, test_accuracies))

    return results


def train_combined_model():
    model = [
        LinearTransform(input_dim=20, output_dim=15, activation='relu'),
        RBFTransform(input_dim=15, output_dim=17),
        LinearTransform(input_dim=17, output_dim=13, activation="tanh"),
        RBFTransform(input_dim=13, output_dim=20),
        LinearTransform(input_dim=20, output_dim=3, activation='identity')
    ]

    train_losses, test_accuracies = train_model(model, X_train, X_test, y_test)
    return train_losses, test_accuracies


linear_results = train_linear_models()
plot_results(linear_results, 'Linear Models')

rbf_results = train_rbf_models()
plot_results(rbf_results, 'RBF Models')

combined_losses, combined_accuracies = train_combined_model()
plt.figure(figsize=(12, 5))
plt.subplot(1, 2, 1)
plt.plot(combined_losses, label='Combined Model')
plt.title('Combined Model - Training Loss')
plt.xlabel('Epoch')
plt.ylabel('Loss')
plt.legend()

plt.subplot(1, 2, 2)
plt.plot(combined_accuracies, label='Combined Model')
plt.title('Combined Model - Test Accuracy')
plt.xlabel('Epoch')
plt.ylabel('Accuracy')
plt.legend()

plt.savefig('combine.png')
