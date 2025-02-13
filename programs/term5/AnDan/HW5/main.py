import pandas as pd
import numpy as np
from matplotlib import pyplot as plt
from sklearn.decomposition import PCA
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.ensemble import RandomForestClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.manifold import TSNE
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score, f1_score, precision_score, recall_score
from sklearn.cluster import KMeans
from sklearn.metrics import silhouette_score, adjusted_rand_score as ari
from scipy.stats import chi2_contingency
from sklearn.feature_selection import SelectKBest, chi2, RFE
from sklearn.svm import SVC

pd.set_option('display.max_columns', None)
pd.set_option('display.max_rows', None)
pd.set_option('display.expand_frame_repr', False)


df = pd.read_csv("SMS.tsv", sep='\t', header=0, names=['label', 'text'])
df['label'] = df['label'].map({'ham': 0, 'spam': 1})

vectorizer = CountVectorizer(max_features=1000, stop_words='english')
X = vectorizer.fit_transform(df['text']).toarray()
y = df['label']
feature_names = vectorizer.get_feature_names_out()


def chi2_manual(X, y):
    scores = []
    for i in range(X.shape[1]):
        contingency_table = np.array([
            [np.sum((X[:, i] > 0) & (y == 0)), np.sum((X[:, i] > 0) & (y == 1))],
            [np.sum((X[:, i] == 0) & (y == 0)), np.sum((X[:, i] == 0) & (y == 1))]
        ])
        chi2, _, _, _ = chi2_contingency(contingency_table)
        scores.append(chi2)
    return np.argsort(scores)[-30:]


filter_indices_manual = chi2_manual(X, y)
selected_words_filter_manual = feature_names[filter_indices_manual]


def calculate_feature_importance(X, y):
    model = RandomForestClassifier(random_state=42)
    model.fit(X, y)
    importances = model.feature_importances_
    return importances


importances = calculate_feature_importance(X, y)
embedded_indices_manual = np.argsort(importances)[-30:]
selected_words_embedded_manual = feature_names[embedded_indices_manual]


def recursive_feature_elimination(X, y, model, n_features, step=10):
    selected = list(range(X.shape[1]))
    while len(selected) > n_features:
        X_temp = X[:, selected]
        model.fit(X_temp, y)

        if hasattr(model, "coef_"):
            scores = np.abs(model.coef_).mean(axis=0)
        elif hasattr(model, "feature_importances_"):
            scores = model.feature_importances_
        else:
            raise ValueError("Model does not support feature importance retrieval")

        worst_indices = np.argsort(scores)[:step]
        selected = [selected[i] for i in range(len(selected)) if i not in worst_indices]
    return selected


wrapper_indices_manual = recursive_feature_elimination(
    X, y, LogisticRegression(max_iter=1000, random_state=42), 30
)
selected_words_rfe_manual = feature_names[wrapper_indices_manual]


kbest = SelectKBest(score_func=chi2, k=30)
X_kbest = kbest.fit_transform(X, y)
kbest_indices = kbest.get_support(indices=True)
selected_words_kbest = feature_names[kbest_indices]


model_rf = RandomForestClassifier(random_state=42)
model_rf.fit(X, y)
importances_library = model_rf.feature_importances_
embedded_indices_library = np.argsort(importances_library)[-30:]
selected_words_embedded_library = feature_names[embedded_indices_library]


model_rfe = LogisticRegression(max_iter=1000, random_state=42)
rfe = RFE(estimator=model_rfe, n_features_to_select=30)
X_rfe_library = rfe.fit_transform(X, y)
wrapper_indices_library = rfe.get_support(indices=True)
selected_words_rfe_library = feature_names[wrapper_indices_library]


print("30 наиболее значимых признаков (слов):")
print("\nФильтровый метод (Chi-Square):")
print(selected_words_filter_manual)

print("\nВстроенный метод (Random Forest):")
print(selected_words_embedded_manual)

print("\nМетод-обёртка (RFE):")
print(selected_words_rfe_manual)

print("\nФильтровый метод (SelectKBest):")
print(selected_words_kbest)

print("\nВстроенный метод (Random Forest, библиотека):")
print(selected_words_embedded_library)

print("\nМетод-обёртка (RFE, библиотека):")
print(selected_words_rfe_library)

classifiers = {
    'Logistic Regression': LogisticRegression(max_iter=1000),
    'Random Forest': RandomForestClassifier(),
    'SVM': SVC(kernel='linear')
}

metrics = {'Accuracy': accuracy_score, 'F1-Score': f1_score, 'Precision': precision_score, 'Recall': recall_score}
results = {}

for name, clf in classifiers.items():
    scores = {}
    for subset, indices in zip(
            ['Original', 'Manual Filter', 'Library Filter', 'Manual Embedded', 'Library Embedded', 'Manual Wrapper',
             'Library Wrapper'],
            [None, filter_indices_manual, kbest_indices, embedded_indices_manual, embedded_indices_library,
             wrapper_indices_manual, wrapper_indices_library]
    ):
        if indices is None:
            X_subset = X
        else:
            X_subset = X[:, indices]

        X_train, X_test, y_train, y_test = train_test_split(X_subset, y, test_size=0.3, random_state=42)
        clf.fit(X_train, y_train)
        y_pred = clf.predict(X_test)

        subset_scores = {metric_name: metric(y_test, y_pred) for metric_name, metric in metrics.items()}
        scores[subset] = subset_scores
    results[name] = scores

results_formatted = []

for name, clf_results in results.items():
    for subset, scores in clf_results.items():
        row = {
            "Model": name,
            "Feature Selection": subset,
            **scores
        }
        results_formatted.append(row)

df_results = pd.DataFrame(results_formatted)
print("\nКлассификационные метрики для всех моделей и методов выбора признаков:")
print(df_results)


kmeans = KMeans(n_clusters=2)
clusters_original = kmeans.fit_predict(X)
score_original = silhouette_score(X, clusters_original)
ari_original = ari(y, clusters_original)

X_filter_manual = X[:, filter_indices_manual]
clusters_filter_manual = kmeans.fit_predict(X_filter_manual)
score_filter_manual = silhouette_score(X_filter_manual, clusters_filter_manual)
ari_filter_manual = ari(y, clusters_filter_manual)

X_kbest_filter = X[:, kbest_indices]
clusters_filter_library = kmeans.fit_predict(X_kbest_filter)
score_filter_library = silhouette_score(X_kbest_filter, clusters_filter_library)
ari_filter_library = ari(y, clusters_filter_library)

X_embedded_manual = X[:, embedded_indices_manual]
clusters_embedded_manual = kmeans.fit_predict(X_embedded_manual)
score_embedded_manual = silhouette_score(X_embedded_manual, clusters_embedded_manual)
ari_embedded_manual = ari(y, clusters_embedded_manual)

X_embedded_library = X[:, embedded_indices_library]
clusters_embedded_library = kmeans.fit_predict(X_embedded_library)
score_embedded_library = silhouette_score(X_embedded_library, clusters_embedded_library)
ari_embedded_library = ari(y, clusters_embedded_library)

X_rfe_manual = X[:, wrapper_indices_manual]
clusters_rfe_manual = kmeans.fit_predict(X_rfe_manual)
score_rfe_manual = silhouette_score(X_rfe_manual, clusters_rfe_manual)
ari_rfe_manual = ari(y, clusters_rfe_manual)

X_rfe_library = X[:, wrapper_indices_library]
clusters_rfe_library = kmeans.fit_predict(X_rfe_library)
score_rfe_library = silhouette_score(X_rfe_library, clusters_rfe_library)
ari_rfe_library = ari(y, clusters_rfe_library)

print("Clustering Scores:")
print(f"Original: Silhouette = {score_original}, ARI = {ari_original}")
print(f"Manual Filter: Silhouette = {score_filter_manual}, ARI = {ari_filter_manual}")
print(f"Library Filter: Silhouette = {score_filter_library}, ARI = {ari_filter_library}")
print(f"Manual Embedded: Silhouette = {score_embedded_manual}, ARI = {ari_embedded_manual}")
print(f"Library Embedded: Silhouette = {score_embedded_library}, ARI = {ari_embedded_library}")
print(f"Manual Wrapper: Silhouette = {score_rfe_manual}, ARI = {ari_rfe_manual}")
print(f"Library Wrapper: Silhouette = {score_rfe_library}, ARI = {ari_rfe_library}")

pca = PCA(n_components=2)
X_pca = pca.fit_transform(X)

plt.figure(figsize=(12, 6))
plt.scatter(X_pca[:, 0], X_pca[:, 1], c=y, cmap='viridis', s=10)
plt.colorbar(label='Label')
plt.title("PCA: Visualization of the data")
plt.xlabel("PCA Component 1")
plt.ylabel("PCA Component 2")
plt.savefig('1.png')

tsne = TSNE(n_components=2)
X_tsne = tsne.fit_transform(X)

plt.figure(figsize=(12, 6))
plt.scatter(X_tsne[:, 0], X_tsne[:, 1], c=y, cmap='viridis', s=10)
plt.colorbar(label='Label')
plt.title("t-SNE: Visualization of the data")
plt.xlabel("t-SNE Component 1")
plt.ylabel("t-SNE Component 2")
plt.savefig('2.png')

