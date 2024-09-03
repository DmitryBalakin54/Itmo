import numpy as np
import matplotlib.pyplot as plt
from sklearn import svm

# Данные
n = 20
X = np.array([[np.random.uniform(-5, 5), np.random.uniform(-5, 5)] for i in range(n)])
y = np.array([-1 if X[i][0] - X[i][1] > 0 else 1 for i in range(n)])

# Обучение модели SVM
clf = svm.SVC(kernel='linear')
clf.fit(X, y)

# Построение графика
plt.scatter(X[:, 0], X[:, 1], c=y, cmap=plt.cm.Paired)

# Построение гиперплоскости
ax = plt.gca()
xlim = ax.get_xlim()
ylim = ax.get_ylim()
xx = np.linspace(xlim[0], xlim[1], 30)
yy = np.linspace(ylim[0], ylim[1], 30)
YY, XX = np.meshgrid(yy, xx)
xy = np.vstack([XX.ravel(), YY.ravel()]).T
Z = clf.decision_function(xy).reshape(XX.shape)

ax.contour(XX, YY, Z, colors='k', levels=[-1, 0, 1], alpha=0.5,
           linestyles=['--', '-', '--'])

plt.scatter(clf.support_vectors_[:, 0], clf.support_vectors_[:, 1], s=100,
            linewidth=1, facecolors='none', edgecolors='k')
plt.show()
