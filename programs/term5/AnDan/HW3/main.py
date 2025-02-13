import numpy as np
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score
from sklearn.preprocessing import StandardScaler
from scipy.spatial.distance import cdist
import matplotlib.pyplot as plt
import warnings


class LinRegression:
    def __init__(self, alpha=1.0):
        self.w = None
        self.alpha = alpha

    def fit(self, X, y):
        n, d = X.shape
        I = np.eye(d)
        I[0, 0] = 0
        self.w = np.linalg.inv(X.T @ X + self.alpha * I) @ X.T @ y

    def predict(self, X):
        return X @ self.w


class LinKlass:
    def __init__(self, lr=0.01, epochs=1000, l1=0.0, l2=0.0, loss='log'):
        self.w = None
        self.lr = lr
        self.epochs = epochs
        self.l1 = l1
        self.l2 = l2
        self.loss = loss

    def fit(self, X, y):
        n, d = X.shape
        y = y * 2 - 1
        self.w = np.zeros(d)

        for _ in range(self.epochs):
            margins = y * (X @ self.w)
            grad_loss = None
            if self.loss == 'log':
                grad_loss = -((1 - 1 / (1 + np.exp(-margins))) * y)[:, None] * X
            elif self.loss == 'hinge':
                grad_loss = -((margins < 1).astype(int) * y)[:, None] * X
            elif self.loss == 'exp':
                grad_loss = -(np.exp(-margins) * y)[:, None] * X

            gradients = np.mean(grad_loss, axis=0)
            gradients += self.l1 * np.sign(self.w) + self.l2 * self.w
            self.w -= self.lr * gradients

    def predict(self, X):
        return (X @ self.w > 0).astype(int)


class SVM:
    def __init__(self, lr=0.01, epochs=1000, C=1.0, kernel_type='linear', degree=3, gamma=1.0):
        self.alpha = None
        self.b = 0
        self.lr = lr
        self.epochs = epochs
        self.C = C
        self.kernel_type = kernel_type
        self.degree = degree
        self.gamma = gamma
        self.X = None
        self.y = None

    def kernel(self, X, Y):
        if self.kernel_type == 'linear':
            return X @ Y.T
        elif self.kernel_type == 'poly':
            return (X @ Y.T + 1) ** self.degree
        elif self.kernel_type == 'rbf':
            return np.exp(-self.gamma * cdist(X, Y, 'sqeuclidean'))

    def fit(self, X, y):
        n, _ = X.shape
        y = y * 2 - 1
        self.alpha = np.zeros(n)
        self.b = 0

        self.X = X
        self.y = y

        K = self.kernel(X, X)

        for _ in range(self.epochs):
            i = np.random.randint(0, n)
            margin = y[i] * (np.sum(self.alpha * y * K[:, i]) + self.b)
            if margin < 1:
                self.alpha[i] += self.lr * (1 - margin)
                self.alpha[i] = np.clip(self.alpha[i], 0, self.C)

    def predict(self, X):
        K_test = self.kernel(X, self.support_vectors_)
        decision = K_test @ (self.support_vector_alphas_ * self.support_vector_labels_) + self.b
        return (decision > 0).astype(int)

    @property
    def support_vector_alphas_(self):
        return self.alpha[self.alpha > 1e-6]

    @property
    def support_vectors_(self):
        return self.X[self.alpha > 1e-6]

    @property
    def support_vector_labels_(self):
        return self.y[self.alpha > 1e-6]


def plot_learning_curve(algorithm, X_train, y_train, X_test, y_test, label, is_lin=False):
    train_errors, test_errors = [], []
    ci_train, ci_test = [], []

    for frac in np.linspace(0.1, 0.9999, 10):
        accuracies_train, accuracies_test = [], []
        for _ in range(5):
            X_train_frac, _, y_train_frac, _ = train_test_split(X_train, y_train, train_size=frac)
            algorithm.fit(X_train_frac, y_train_frac)

            if isinstance(algorithm, LinRegression):
                train_preds = (algorithm.predict(X_train_frac) > 0.5).astype(int)
                test_preds = (algorithm.predict(X_test) > 0.5).astype(int)
            else:
                train_preds = algorithm.predict(X_train_frac)
                test_preds = algorithm.predict(X_test)

            accuracies_train.append(accuracy_score(y_train_frac, train_preds))
            accuracies_test.append(accuracy_score(y_test, test_preds))

        mean_acc_train = np.mean(accuracies_train)
        mean_acc_test = np.mean(accuracies_test)

        ci_train.append(1.96 * np.std(accuracies_train) / np.sqrt(len(accuracies_train)))
        ci_test.append(1.96 * np.std(accuracies_test) / np.sqrt(len(accuracies_test)))

        train_errors.append(1 - mean_acc_train)
        test_errors.append(1 - mean_acc_test)

    if is_lin:
        lin_pred = (algorithm.predict(X_test) > 0.5).astype(int)
        lin_accuracy = accuracy_score(y_test, lin_pred)
        plt.axhline(y=1 - lin_accuracy, color='black', linestyle='--', label=f'{label} (Test Mean)')
        return

    plt.plot(np.linspace(0.1, 0.9999, 10), test_errors, label=f'{label} (Test)')
    plt.plot(np.linspace(0.1, 0.9999, 10), train_errors, '--', label=f'{label} (Train)')
    plt.fill_between(np.linspace(0.1, 0.9999, 10),
                     [e - c for e, c in zip(test_errors, ci_test)],
                     [e + c for e, c in zip(test_errors, ci_test)], alpha=0.2)
    plt.fill_between(np.linspace(0.1, 0.9999, 10),
                     [e - c for e, c in zip(train_errors, ci_train)],
                     [e + c for e, c in zip(train_errors, ci_train)], alpha=0.2)


if __name__ == '__main__':
    warnings.filterwarnings("ignore")
    data = pd.read_csv('xatab_rpg__data.csv')

    data['views'] = np.log1p(data['views'])
    if 'downloads' in data.columns:
        data['downloads'] = np.log1p(data['downloads'])
    if 'release_date' in data.columns:
        data['release_date'] = np.log1p(data['release_date'] - data['release_date'].min())

    data = data[data['views'] < np.log1p(100_000)]
    target = 'views'
    data['target'] = (data[target] > np.log1p(35_000))

    data_columns = [col for col in data.columns if
                    col.startswith('genre_') or
                    'type' in col or
                    ('language' in col and 'rus' in col) or
                    'downloads' in col
                    ]

    X = data[data_columns].values
    y = data['target'].values

    scaler = StandardScaler()
    X = scaler.fit_transform(X)

    X_train, X_test, y_train, y_test, idx_train, idx_test = train_test_split(
        X, y, data.index, test_size=0.5
    )

    best_alpha = None
    best_lin_accuracy = 0

    for alpha in [0.1, 1.0, 10.0, 100.0]:
        linreg = LinRegression(alpha=alpha)
        linreg.fit(X_train, y_train)
        ridge_preds = linreg.predict(X_test)
        ridge_binary_preds = (ridge_preds > 0.5).astype(int)
        acc = accuracy_score(y_test, ridge_binary_preds)
        if acc > best_lin_accuracy:
            best_lin_accuracy = acc
            best_alpha = alpha

    print(f"LinReg: Best alpha = {best_alpha}, Accuracy = {best_lin_accuracy:.3f}")

    best_lk_params = None
    best_lk_accuracy = 0

    for lr in [0.001, 0.01, 0.1, 0.5]:
        for l1 in [0.0, 0.01, 0.1, 0.5]:
            for l2 in [0.0, 0.01, 0.1, 0.5]:
                for los in ['log', 'exp', 'hinge']:
                    linklass = LinKlass(lr=lr, epochs=1000, l1=l1, l2=l2, loss=los)
                    linklass.fit(X_train, y_train)
                    logreg_preds = linklass.predict(X_test)
                    acc = accuracy_score(y_test, logreg_preds)
                    if acc > best_lk_accuracy:
                        best_lk_accuracy = acc
                        best_lk_params = {'lr': lr, 'l1': l1, 'l2': l2, 'loss': los}

    print(f"LinKlass: Best params = {best_lk_params}, Accuracy = {best_lk_accuracy:.3f}")

    best_svm_params = None
    best_svm_accuracy = 0

    for C in [0.1, 1.0, 10.0, 50.0, 100.0]:
        for gamma in [0.01, 0.1, 1.0]:
            for ker in ['rbf', 'linear', 'poly']:
                svm = SVM(lr=0.01, epochs=1000, C=C, kernel_type=ker, gamma=gamma)
                svm.fit(X_train, y_train)
                svm_preds = svm.predict(X_test)
                acc = accuracy_score(y_test, svm_preds)
                if acc > best_svm_accuracy:
                    best_svm_accuracy = acc
                    best_svm_params = {'C': C, 'gamma': gamma, 'kernel_type': ker}

    print(f"SVM: Best params = {best_svm_params}, Accuracy = {best_svm_accuracy:.3f}")

    plt.figure(figsize=(10, 6))
    linklass = LinKlass(**best_lk_params)
    svmm = SVM(C=best_svm_params['C'], gamma=best_svm_params['gamma'], kernel_type=best_svm_params['kernel_type'])
    linreg = LinRegression(alpha=best_alpha)

    plot_learning_curve(linreg, X_train, y_train, X_test, y_test, 'LinReg', is_lin=True)
    plot_learning_curve(linklass, X_train, y_train, X_test, y_test, 'LinKlass')
    plot_learning_curve(svmm, X_train, y_train, X_test, y_test, 'SVM')

    plt.legend()
    plt.xlabel("Training set size")
    plt.ylabel("Error rate")
    plt.title("Learning Curves")
    plt.savefig('res.png')
    plt.show()

    game_titles = data.loc[idx_test, 'title']

    results_df = pd.DataFrame({
        'Game Title': game_titles,
        'Value': [1 if i else 0 for i in y_test],
        'LinKlass': logreg_preds,
        'SVM': svm_preds,
        'LinReg': ridge_binary_preds,
    })

    results_df.to_excel('game_predictions.xlsx', index=False)
