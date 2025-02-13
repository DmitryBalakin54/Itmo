import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from sklearn.ensemble import RandomForestClassifier, GradientBoostingClassifier
from sklearn.metrics import accuracy_score
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.tree import DecisionTreeClassifier


class DecisionTree:
    def __init__(self, max_depth=None, min_samples_split=2, min_samples_leaf=1):
        self.max_depth = max_depth
        self.min_samples_split = min_samples_split
        self.min_samples_leaf = min_samples_leaf
        self.tree = None

    def fit(self, X, y):
        self.tree = self._build_tree(X, y, depth=0)

    def _build_tree(self, X, y, depth):
        num_samples, num_features = X.shape
        if num_samples >= self.min_samples_split and (self.max_depth is None or depth < self.max_depth):
            best_split = self._get_best_split(X, y, num_features)
            if best_split['mse'] > 0:
                left_tree = self._build_tree(X[best_split['indices_left']], y[best_split['indices_left']], depth + 1)
                right_tree = self._build_tree(X[best_split['indices_right']], y[best_split['indices_right']], depth + 1)
                return {
                    'feature': best_split['feature'],
                    'threshold': best_split['threshold'],
                    'left': left_tree,
                    'right': right_tree
                }
        return {'value': np.mean(y)}

    def _get_best_split(self, X, y, num_features):
        best_split = {'mse': 0}
        for feature in range(num_features):
            thresholds = np.unique(X[:, feature])
            for threshold in thresholds:
                indices_left = X[:, feature] <= threshold
                indices_right = X[:, feature] > threshold
                if len(indices_left) < self.min_samples_leaf or len(indices_right) < self.min_samples_leaf:
                    continue
                mse = self._information_mse(y, indices_left, indices_right)
                if mse > best_split['mse']:
                    best_split = {
                        'feature': feature,
                        'threshold': threshold,
                        'mse': mse,
                        'indices_left': indices_left,
                        'indices_right': indices_right
                    }
        return best_split

    def _information_mse(self, y, indices_left, indices_right):
        parent_loss = self._mse(y)
        n = len(y)
        n_left = np.sum(indices_left)
        n_right = np.sum(indices_right)
        if n_left == 0 or n_right == 0:
            return 0
        child_loss = (n_left / n) * self._mse(y[indices_left]) + (n_right / n) * self._mse(y[indices_right])
        return parent_loss - child_loss

    def _mse(self, y):
        return np.mean((y - np.mean(y)) ** 2)

    def predict(self, X):
        return np.array([self._predict(inputs, self.tree) for inputs in X])

    def _predict(self, inputs, tree):
        if 'value' in tree:
            return tree['value']
        feature = tree['feature']
        threshold = tree['threshold']
        if inputs[feature] <= threshold:
            return self._predict(inputs, tree['left'])
        else:
            return self._predict(inputs, tree['right'])


class RandomForest:
    def __init__(self, n_estimators=10, max_depth=None, min_samples_split=2, min_samples_leaf=1):
        self.n_estimators = n_estimators
        self.max_depth = max_depth
        self.min_samples_split = min_samples_split
        self.min_samples_leaf = min_samples_leaf
        self.trees = []
        self.subsample_size = None

    def fit(self, X, y):
        self.subsample_size = len(y) // 2
        for _ in range(self.n_estimators):
            indices = np.random.choice(len(y), self.subsample_size, replace=True)
            tree = DecisionTree(
                max_depth=self.max_depth,
                min_samples_split=self.min_samples_split,
                min_samples_leaf=self.min_samples_leaf
            )
            tree.fit(X[indices], y[indices])
            self.trees.append(tree)

    def predict(self, X):
        predictions = np.array([tree.predict(X) for tree in self.trees])
        return np.mean(predictions, axis=0) > 0.5


class Boosting:
    def __init__(self, n_estimators=10, learning_rate=0.1, max_depth=3):
        self.n_estimators = n_estimators
        self.learning_rate = learning_rate
        self.max_depth = max_depth
        self.models = []

    def fit(self, X, y):
        pred = np.zeros(len(y))
        for _ in range(self.n_estimators):
            residual = y - self._sigmoid(pred)

            tree = DecisionTree(max_depth=self.max_depth)
            tree.fit(X, residual)

            predictions = tree.predict(X)
            pred += self.learning_rate * predictions

            self.models.append(tree)

    def predict(self, X):
        pred = np.zeros(X.shape[0])
        for tree in self.models:
            pred += self.learning_rate * tree.predict(X)
        return (self._sigmoid(pred) > 0.5).astype(int)

    @staticmethod
    def _sigmoid(z):
        return 1 / (1 + np.exp(-z))


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

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.5)

param_grid = {
    'min_samples_split': [2, 5, 7, 10, 12, 15],
    'min_samples_leaf': [1, 2, 4, 6, 8, 10, 12]
}


def get_tree_height(tree):
    if 'value' in tree:
        return 0
    return 1 + max(get_tree_height(tree['left']), get_tree_height(tree['right']))


library_heights_split, library_heights_leaf = [], []
custom_heights_split, custom_heights_leaf = [], []

for split in param_grid['min_samples_split']:
    lib_dt = DecisionTreeClassifier(max_depth=None, min_samples_split=split)
    lib_dt.fit(X_train, y_train)
    library_heights_split.append(lib_dt.tree_.max_depth)

    custom_dt = DecisionTree(max_depth=None, min_samples_split=split)
    custom_dt.fit(X_train, y_train)
    custom_heights_split.append(get_tree_height(custom_dt.tree))

plt.figure(figsize=(10, 6))
plt.plot(param_grid['min_samples_split'], library_heights_split, label="Library Implementation", marker="o")
plt.plot(param_grid['min_samples_split'], custom_heights_split, label="Custom Implementation", marker="x")
plt.xlabel("min_samples_split")
plt.ylabel("Tree Height")
plt.title("Tree Height vs min_samples_split")
plt.legend()
plt.grid()
plt.savefig("TreeHeight_min_samples_split.png")

for leaf in param_grid['min_samples_leaf']:
    lib_dt = DecisionTreeClassifier(max_depth=None, min_samples_leaf=leaf)
    lib_dt.fit(X_train, y_train)
    library_heights_leaf.append(lib_dt.tree_.max_depth)

    custom_dt = DecisionTree(max_depth=None, min_samples_leaf=leaf)
    custom_dt.fit(X_train, y_train)
    custom_heights_leaf.append(get_tree_height(custom_dt.tree))

plt.figure(figsize=(10, 6))
plt.plot(param_grid['min_samples_leaf'], library_heights_leaf, label="Library Implementation", marker="o")
plt.plot(param_grid['min_samples_leaf'], custom_heights_leaf, label="Custom Implementation", marker="x")
plt.xlabel("min_samples_leaf")
plt.ylabel("Tree Height")
plt.title("Tree Height vs min_samples_leaf")
plt.legend()
plt.grid()
plt.savefig("TreeHeight_min_samples_leaf.png")

plt.figure(figsize=(10, 6))
depths = range(1, 50, 5)
custom_train_acc, custom_test_acc = [], []
lib_train_acc, lib_test_acc = [], []

for depth in depths:
    print(f't: {depth}')
    custom_dt = DecisionTree(max_depth=depth)
    custom_dt.fit(X_train, y_train)
    custom_train_acc.append(accuracy_score(y_train, custom_dt.predict(X_train) > 0.5))
    custom_test_acc.append(accuracy_score(y_test, custom_dt.predict(X_test) > 0.5))

    lib_dt = DecisionTreeClassifier(max_depth=depth)
    lib_dt.fit(X_train, y_train)
    lib_train_acc.append(accuracy_score(y_train, lib_dt.predict(X_train)))
    lib_test_acc.append(accuracy_score(y_test, lib_dt.predict(X_test)))

plt.plot(depths, custom_train_acc, label='Custom Train', linestyle='--', marker='o')
plt.plot(depths, custom_test_acc, label='Custom Test', linestyle='-', marker='o')
plt.plot(depths, lib_train_acc, label='Library Train', linestyle='--', marker='x')
plt.plot(depths, lib_test_acc, label='Library Test', linestyle='-', marker='x')
plt.xlabel("Depth")
plt.ylabel("Accuracy")
plt.title("Decision Tree Performance")
plt.legend()
plt.grid()
plt.savefig('DecisionTree.png')

plt.figure(figsize=(10, 6))
n_estimators = range(1, 50, 5)
custom_train_acc, custom_test_acc = [], []
lib_train_acc, lib_test_acc = [], []

for n in n_estimators:
    print(f'f: {n}')
    custom_rf = RandomForest(n_estimators=n, max_depth=5)
    custom_rf.fit(X_train, y_train)
    custom_train_acc.append(accuracy_score(y_train, custom_rf.predict(X_train)))
    custom_test_acc.append(accuracy_score(y_test, custom_rf.predict(X_test)))

    lib_rf = RandomForestClassifier(n_estimators=n, max_depth=5)
    lib_rf.fit(X_train, y_train)
    lib_train_acc.append(accuracy_score(y_train, lib_rf.predict(X_train)))
    lib_test_acc.append(accuracy_score(y_test, lib_rf.predict(X_test)))

plt.plot(n_estimators, custom_train_acc, label='Custom Train', linestyle='--', marker='o')
plt.plot(n_estimators, custom_test_acc, label='Custom Test', linestyle='-', marker='o')
plt.plot(n_estimators, lib_train_acc, label='Library Train', linestyle='--', marker='x')
plt.plot(n_estimators, lib_test_acc, label='Library Test', linestyle='-', marker='x')
plt.xlabel("Number of Estimators")
plt.ylabel("Accuracy")
plt.title("Random Forest Performance")
plt.legend()
plt.grid()
plt.savefig('RandomForest.png')

plt.figure(figsize=(10, 6))
n_estimators = range(1, 100, 10)
custom_train_acc, custom_test_acc = [], []
lib_train_acc, lib_test_acc = [], []

for n in n_estimators:
    print(f'b: {n}')
    custom_boost = Boosting(n_estimators=n, learning_rate=0.1)
    custom_boost.fit(X_train, y_train)
    custom_train_acc.append(accuracy_score(y_train, custom_boost.predict(X_train)))
    custom_test_acc.append(accuracy_score(y_test, custom_boost.predict(X_test)))

    lib_boost = GradientBoostingClassifier(n_estimators=n, learning_rate=0.1)
    lib_boost.fit(X_train, y_train)
    lib_train_acc.append(accuracy_score(y_train, lib_boost.predict(X_train)))
    lib_test_acc.append(accuracy_score(y_test, lib_boost.predict(X_test)))

plt.plot(n_estimators, custom_train_acc, label='Custom Train', linestyle='--', marker='o')
plt.plot(n_estimators, custom_test_acc, label='Custom Test', linestyle='-', marker='o')
plt.plot(n_estimators, lib_train_acc, label='Library Train', linestyle='--', marker='x')
plt.plot(n_estimators, lib_test_acc, label='Library Test', linestyle='-', marker='x')
plt.xlabel("Number of Estimators")
plt.ylabel("Accuracy")
plt.title("Boosting Performance")
plt.legend()
plt.grid()
plt.savefig('Boosting.png')
