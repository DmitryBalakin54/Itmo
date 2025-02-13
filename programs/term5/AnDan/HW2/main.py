from copy import deepcopy

import numpy as np
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import mean_squared_error
from scipy.spatial.distance import cdist
import matplotlib.pyplot as plt
from itertools import product
import openpyxl


class KNN:
    def __init__(self, n_neighbors=5, metric='euclidean', kernel='uniform', alpha=1, beta=2, radius=None):
        self.n_neighbors = n_neighbors
        self.metric = metric if metric != 'manhattan' else 'cityblock'
        self.kernel = kernel
        self.alpha = alpha
        self.beta = beta
        self.radius = radius
        self.X_train = None
        self.y_train = None

    def _get_kernel_weights(self, distances):
        if self.kernel == 'uniform':
            return np.ones_like(distances)
        elif self.kernel == 'gaussian':
            return np.exp(-distances ** 2)
        elif self.kernel == 'general':
            return (1 - np.abs(distances) ** self.alpha) ** self.beta
        elif self.kernel == 'triangular':
            return np.maximum(1 - distances, 0)
        else:
            return np.ones_like(distances)

    def fit(self, X, y):
        self.X_train = X
        self.y_train = y

    def predict(self, X, log=False):
        predictions = []
        for x in X:
            distances = cdist([x], self.X_train, metric=self.metric).flatten()

            if self.radius:
                neighbors_indices = np.where(distances < self.radius)[0]
                if log:
                    print(f'indexes={neighbors_indices}, radius={self.radius}')
            else:
                neighbors_indices = np.argsort(distances)[:self.n_neighbors]

            if len(neighbors_indices) == 0:
                if log:
                    print(self.radius)
                prediction = np.mean(self.y_train)
            else:
                selected_distances = distances[neighbors_indices]
                selected_y = self.y_train[neighbors_indices]

                weights = self._get_kernel_weights(selected_distances)
                if weights.sum() == 0:
                    prediction = selected_y.mean()
                else:
                    prediction = np.dot(weights, selected_y) / weights.sum()

            predictions.append(prediction)
        return np.array(predictions)


def train_and_evaluate_knn(anomalies_train_low, anomalies_test_low, best_params_low, X_train_orig, y_train_orig, X_test_orig, y_test_orig, titles_test_orig,
                           output_prefix, log_suf):
    X_train_cleaned = np.delete(X_train_orig, anomalies_train_low, axis=0)
    y_train_cleaned = np.delete(y_train_orig, anomalies_train_low, axis=0)
    X_test_cleaned = np.delete(X_test_orig, anomalies_test_low, axis=0)
    y_test_cleaned = np.delete(y_test_orig, anomalies_test_low, axis=0)
    titles_test_cleaned = np.delete(titles_test_orig, anomalies_test_low, axis=0)

    knn_model_low = KNN(**best_params_low)
    knn_model_low.fit(X_train_cleaned, y_train_cleaned)

    y_pred_train_low = knn_model_low.predict(X_train_cleaned)
    y_pred_test_low = knn_model_low.predict(X_test_cleaned)
    mse_train_low = mean_squared_error(y_train_cleaned, y_pred_train_low)
    mse_test_low = mean_squared_error(y_test_cleaned, y_pred_test_low)

    print(f"\nОшибка MSE на очищенном обучающем наборе ({log_suf}): {mse_train_low}")
    print(f"Ошибка MSE на очищеном тестовом наборе ({log_suf}): {mse_test_low}")

    y_pred_test_original_low = np.expm1(y_pred_test_low)
    y_test_original_low = np.expm1(y_test_cleaned)

    predicted_vs_actual_low = pd.DataFrame({
        'Название игры': titles_test_cleaned,
        'Истинные значения': y_test_original_low.astype(int),
        'Предсказанные значения': y_pred_test_original_low.astype(int)
    })

    predicted_vs_actual_low['Отношение'] = predicted_vs_actual_low.apply(
        lambda row: max(row['Истинные значения'] / row['Предсказанные значения'],
                        row['Предсказанные значения'] / row['Истинные значения']),
        axis=1
    ).astype(float)

    predicted_vs_actual_low.to_csv(f'{output_prefix}.csv', sep='\t', index=False, header=True)
    predicted_vs_actual_low.to_excel(f'{output_prefix}.xlsx', index=False)


def lowess_anomaly_detection(X, y, frac=0.2):
    sorted_indices = np.argsort(X[:, 0])
    X_sorted, y_sorted = X[sorted_indices], y[sorted_indices]
    n = len(y_sorted)
    predictions = np.zeros(n)
    for i in range(n):
        distances = np.abs(X_sorted[:, 0] - X_sorted[i, 0])
        weights = np.exp(-distances / frac)
        weighted_sum = np.sum(weights * y_sorted) / np.sum(weights)
        predictions[i] = weighted_sum

    residuals = np.abs(y_sorted - predictions)
    threshold = np.percentile(residuals, 95)
    anomalies = sorted_indices[residuals > threshold]
    return anomalies, residuals


def evaluate_and_save_results(cur_knn_model, cur_X_train, cur_y_train, cur_X_test, cur_y_test, cur_titles_test, file_prefix, log_suf=''):
    cur_knn_model.fit(cur_X_train, cur_y_train)

    cur_y_pred_train = cur_knn_model.predict(cur_X_train)
    cur_y_pred_test = cur_knn_model.predict(cur_X_test)
    cur_mse_train = mean_squared_error(cur_y_train, cur_y_pred_train)
    cur_mse_test = mean_squared_error(cur_y_test, cur_y_pred_test)

    print(f"\nОшибка MSE на обучающем наборе ({log_suf}):", cur_mse_train)
    print(f"Ошибка MSE на тестовом наборе ({log_suf}):", cur_mse_test)

    cur_y_pred_test_original = np.expm1(cur_y_pred_test)
    cur_y_test_original = np.expm1(cur_y_test)

    cur_predicted_vs_actual = pd.DataFrame({
        'Название игры': cur_titles_test,
        'Истинные значения': cur_y_test_original.astype(int),
        'Предсказанные значения': cur_y_pred_test_original.astype(int)
    })
    cur_predicted_vs_actual['Отношение'] = cur_predicted_vs_actual.apply(
        lambda row: max(row['Истинные значения'] / row['Предсказанные значения'],
                        row['Предсказанные значения'] / row['Истинные значения']),
        axis=1
    ).astype(float)

    cur_predicted_vs_actual.to_csv(f'{file_prefix}.csv', sep='\t', index=False, header=True)
    cur_predicted_vs_actual.to_excel(f'{file_prefix}.xlsx', index=False)


if __name__ == '__main__':
    data = pd.read_csv('xatab_rpg__data.csv')

    data['views'] = np.log1p(data['views'])
    if 'downloads' in data.columns:
        data['downloads'] = np.log1p(data['downloads'])

    if 'release_date' in data.columns:
        data['release_date'] = np.log1p(data['release_date'] - data['release_date'].min())

    data = data[data['views'] < np.log1p(100_000)]
    target = 'views'

    # if 'downloads' in data.columns:
    #     data['downloads'] = data['downloads'] * 1
    #
    # if 'release_date' in data.columns:
    #     data['release_date'] = data['release_date'] * 1
    #
    # for col in data.columns:
    #     if 'language' in col and 'rus' in col:
    #         data[col] = data[col] * 1

    data_columns = [col for col in data.columns if
                    col.startswith('genre_')
                    or 'downloads' in col
                    or 'type' in col
                    or 'language' in col and 'rus' in col
                    or 'release_date' in col
                    ]

    features = data[data_columns].copy()
    y = data[target].values
    X = features.values
    titles = data['title'].values

    X_train, X_test, y_train, y_test, titles_train, titles_test = train_test_split(X, y, titles, test_size=0.5)

    scaler = StandardScaler()
    X_train = scaler.fit_transform(X_train)
    X_test = scaler.transform(X_test)

    param_grid_windows = {
        'metric': ['euclidean', 'manhattan'],
        'kernel': ['uniform', 'triangular', 'gaussian', 'general'],
        'alpha': [1, 2],
        'beta': [1, 2],
        'radius': [4.5, 6.0, 9.0, 10.0, 15, 20]
    }

    best_params_windows = None
    best_mse_windows = float('inf')
    window_train_errors = []
    window_test_errors = []
    window_radius_values = []

    for cur_neighbors in product(*param_grid_windows.values()):
        current_params = dict(zip(param_grid_windows.keys(), cur_neighbors))

        knn_model = KNN(**current_params)
        knn_model.fit(X_train, y_train)

        y_pred_train = knn_model.predict(X_train)
        y_pred_test = knn_model.predict(X_test)
        mse_train = mean_squared_error(y_train, y_pred_train)
        mse_test = mean_squared_error(y_test, y_pred_test)

        if mse_test < best_mse_windows:
            best_mse_windows = mse_test
            best_params_windows = current_params

    for cur_radius in param_grid_windows['radius']:
        current_params = best_params_windows.copy()
        current_params['radius'] = cur_radius
        knn_model = KNN(**current_params)
        knn_model.fit(X_train, y_train)

        y_pred_train = knn_model.predict(X_train)
        y_pred_test = knn_model.predict(X_test)
        mse_train = mean_squared_error(y_train, y_pred_train)
        mse_test = mean_squared_error(y_test, y_pred_test)

        window_train_errors.append(mse_train)
        window_test_errors.append(mse_test)
        window_radius_values.append(current_params['radius'])

    print("Лучшие параметры win:", best_params_windows)

    plt.plot(window_radius_values, window_train_errors, label="MSE на обучающем множестве")
    plt.plot(window_radius_values, window_test_errors, label="MSE на тестовом множестве")
    plt.xlabel("Радиус окна")
    plt.ylabel("MSE")
    plt.title("Зависимость MSE от радиуса окна")
    plt.legend()
    plt.grid(True)
    plt.savefig('windows.png')

    param_grid_neighbors = {
        'metric': ['euclidean', 'manhattan'],
        'kernel': ['uniform', 'triangular', 'gaussian', 'general'],
        'alpha': [1, 2],
        'beta': [1, 2],
        'n_neighbors': range(1, 80, 4)
    }

    best_params_neighbors = None
    best_mse_neighbors = float('inf')
    neighbors_train_errors = []
    neighbors_test_errors = []
    neighbors_values = []

    for cur_neighbors in product(*param_grid_neighbors.values()):
        current_params = dict(zip(param_grid_neighbors.keys(), cur_neighbors))

        knn_model = KNN(radius=None, **current_params)
        knn_model.fit(X_train, y_train)

        y_pred_train = knn_model.predict(X_train)
        y_pred_test = knn_model.predict(X_test)
        mse_train = mean_squared_error(y_train, y_pred_train)
        mse_test = mean_squared_error(y_test, y_pred_test)

        if mse_test < best_mse_neighbors:
            best_mse_neighbors = mse_test
            best_params_neighbors = current_params

    for cur_neighbors in param_grid_neighbors['n_neighbors']:
        current_params = best_params_neighbors.copy()
        current_params['n_neighbors'] = cur_neighbors

        knn_model = KNN(radius=None, **current_params)
        knn_model.fit(X_train, y_train)

        y_pred_train = knn_model.predict(X_train)
        y_pred_test = knn_model.predict(X_test)
        mse_train = mean_squared_error(y_train, y_pred_train)
        mse_test = mean_squared_error(y_test, y_pred_test)

        neighbors_train_errors.append(mse_train)
        neighbors_test_errors.append(mse_test)
        neighbors_values.append(current_params['n_neighbors'])

    print("\nЛучшие параметры neigbors:", best_params_neighbors)
    plt.close()
    plt.plot(neighbors_values, neighbors_train_errors, label="MSE на обучающем множестве")
    plt.plot(neighbors_values, neighbors_test_errors, label="MSE на тестовом множестве")
    plt.xlabel("Количество соседей (n_neighbors)")
    plt.ylabel("MSE")
    plt.title("Зависимость MSE от количества соседей")
    plt.legend()
    plt.grid(True)
    plt.savefig('neighbors.png')

    knn_model = KNN(**best_params_neighbors)
    evaluate_and_save_results(knn_model, X_train, y_train, X_test, y_test, titles_test, 'best_predicted_vs_actual', 'neighbors')

    knn_model = KNN(**best_params_windows)
    evaluate_and_save_results(knn_model, X_train, y_train, X_test, y_test, titles_test, 'best_predicted_vs_actual_win', 'win')

    anomalies_train, residuals_train = lowess_anomaly_detection(X_train, y_train, frac=0.2)
    anomalies_test, residuals_test = lowess_anomaly_detection(X_test, y_test, frac=0.2)

    print(f"\nНайдено {len(anomalies_train)} аномалий в обучающем наборе")
    print(f"Найдено {len(anomalies_test)} аномалий в тестовом наборе")

    train_and_evaluate_knn(anomalies_train, anomalies_test, best_params_windows, X_train, y_train, X_test, y_test, titles_test,
                           'best_predicted_vs_actual_win_final', 'win')
    train_and_evaluate_knn(anomalies_train, anomalies_test, best_params_neighbors, X_train, y_train, X_test, y_test, titles_test,
                           'best_predicted_vs_actual_final', 'neighbors')
