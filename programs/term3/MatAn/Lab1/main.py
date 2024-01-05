import time
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.ticker import LinearLocator
from typing import List, Tuple

t: float = time.time()
iterations: int = 0

EPS: float = 0.0000001
A: float = 0.01

# стартовая точка
null_dot: Tuple[float, float] = (1, 1)


# даннная функция
def f(dot: Tuple[float, float]) -> float:
    x, y = dot
    return x ** 2 * y ** 2 * np.log(4 * x ** 2 + y ** 2)


# градиент функции
def grad(dot: Tuple[float, float]) -> Tuple[float, float]:
    x, y = dot
    res_x = 2 * x * y ** 2 * (np.log(4 * x ** 2 + y ** 2) + 4 * x ** 2 / (4 * x ** 2 + y ** 2))
    res_y = 2 * x ** 2 * y * (np.log(4 * x ** 2 + y ** 2) + y ** 2 / (4 * x ** 2 + y ** 2))
    return res_x, res_y


# следующая точка по формуле (xk+1, yk+1) = (xk, yk) ± ak grad f(xk, yk)
def next_dot(dot: Tuple[float, float], a: float) -> Tuple[float, float]:
    gr = grad(dot)
    return dot[0] - a * gr[0], dot[1] - a * gr[1]


# функция, которая ищет последовательность точек при помощи функции next_dot
def get_extr(dot_0: Tuple[float, float]) -> List[Tuple[float, float]]:
    global iterations
    ans = []
    last_dot = dot_0
    a = A
    dot = next_dot(last_dot, a)
    val = np.sqrt((last_dot[0] - dot[0]) ** 2 + (last_dot[1] - dot[1]) ** 2)

    ans.append(last_dot)
    ans.append(dot)

    iterations += 1

    while val >= EPS:
        last_dot = dot
        dot = next_dot(last_dot, a)
        val = np.sqrt((last_dot[0] - dot[0]) ** 2 + (last_dot[1] - dot[1]) ** 2)

        ans.append(dot)
        iterations += 1

    return ans


res: List[Tuple[float, float]] = get_extr(null_dot)

# создаем график
fig, ax = plt.subplots(subplot_kw={"projection": "3d"})

ax.set_zlim(-1.01, 1.01)
ax.zaxis.set_major_locator(LinearLocator(10))
ax.zaxis.set_major_formatter('{x:.02f}')
    
# добавляем точки, которые выдала наша программа
X = [i[0] for i in res]
Y = [i[1] for i in res]
Z = [f(i) for i in res]
ax.scatter(X, Y, Z, color='r', s=2)

# добавляем график функции
X = np.arange(-1.5, 1.5, 0.01)
Y = np.arange(-1.5, 1.5, 0.01)
X, Y = np.meshgrid(X, Y)
Z = f((X, Y))
ax.plot_surface(X, Y, Z, alpha=0.4, color='b')

# добавляем линии уровня
ax.contour(X, Y, Z, levels=20, colors='k', alpha=0.7)

# добавляем четыре точки экстремума
val_x: float = 1 / (2 * np.sqrt(2) * np.e**(1/4))
val_y: float = 1 / (np.sqrt(2) * np.e**(1/4))

X = [val_x, val_x, -val_x, -val_x]
Y = [val_y, -val_y, val_y, -val_y]
Z = [f(dot) for dot in zip(X, Y)]
ax.scatter(X, Y, Z, color='g', s=25)

# добавляем остальные точки экстремума
# это точки экстремума вида (t, 0) или (0, t),
# причем не включены некоторые точки, например точка (0, 0),
# поэтому все эти точки представлены в виде прямых,
# а точки которых быть не должно окружены пустой eps окрестностью для нагладности
eps: float = 0.02

X = [-1.5, -1 / 2 - eps]
Y = [0, 0]
Z = [f(dot) for dot in zip(X, Y)]
ax.plot(X, Y, Z, color='g')

X = [-1 / 2 + eps, 0 - eps]
Y = [0, 0]
Z = [f(dot) for dot in zip(X, Y)]
ax.plot(X, Y, Z, color='g')

X = [0 + eps, 1 / 2 - eps]
Y = [0, 0]
Z = [f(dot) for dot in zip(X, Y)]
ax.plot(X, Y, Z, color='g')

X = [1 / 2 + eps, 1.5]
Y = [0, 0]
Z = [f(dot) for dot in zip(X, Y)]
ax.plot(X, Y, Z, color='g')

X = [0, 0]
Y = [-1.5, -1 - eps]
Z = [f(dot) for dot in zip(X, Y)]
ax.plot(X, Y, Z, color='g')

X = [0, 0]
Y = [-1 + eps, 0 - eps]
Z = [f(dot) for dot in zip(X, Y)]
ax.plot(X, Y, Z, color='g')

X = [0, 0]
Y = [0 + eps, 1 - eps]
Z = [f(dot) for dot in zip(X, Y)]
ax.plot(X, Y, Z, color='g')

X = [0, 0]
Y = [1 + eps, 1.5]
Z = [f(dot) for dot in zip(X, Y)]
ax.plot(X, Y, Z, color='g')

# сохраняем две картинки с разными ракурсами
plt.savefig('fig1.png', dpi=600)
ax.view_init(30, 60)
plt.savefig('fig2.png', dpi=600)


print(f'критерий останова: ||(∆xk,∆yk)|| < {EPS}')
print(f'количество итераций: {iterations}')
print(f'время работы программы(в милисекундах): {int((time.time() - t) * 1000)}')
print(f'полученная точка и значение функции в этой точке: (x, y) = {res[-1]}  f(x, y) = {f(res[-1])}')
print(f'точка экстремума и значение в ней: (x, y) = {val_x, val_y}  f(x, y) = {f((val_x, val_y))}')
print(f'погрешность вычисления: {abs(f(res[-1]) - f((val_x, val_y)))}')