import matplotlib.pyplot as plt
import matplotlib.ticker as ticker

arr1: list[float] = []
with open('res1.log', 'r') as file:
    pars = file.read()[1:].split('\n\n')
    for par in pars:
        arr1.append(sum(map(float, (i if i else 0 for i in par.split('\n')[1:]))) / 10)
    arr1 = arr1[:-1]
    print(arr1)




fig, ax = plt.subplots()
ax.plot(arr1, label='time')

num_categories = 5
total_categories = len(arr1)
indices = [int(i) for i in range(0, total_categories, total_categories // num_categories)]
indices = indices[2:]
indices.append(total_categories - 1)
desired_ticks = [arr1[i] for i in indices]
ax.yaxis.set_major_locator(ticker.FixedLocator(desired_ticks))
ax.legend()

fig.savefig('time1.png')
