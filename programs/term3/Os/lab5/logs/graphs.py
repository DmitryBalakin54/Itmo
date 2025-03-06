import matplotlib.pyplot as plt
import matplotlib.ticker as ticker

mem1 = []
cpu1 = []
virt1 = []
res1 = []


def convert(vals: list[str], ind: int) -> float:
    v = vals[ind]
    if v[-1] == 'g':
        return float(v[:-1]) * 1024 ** 2
    elif v[-1] == 'm':
        return float(v[:-1]) * 1024
    else:
        return float(v)


with open('params1.log', 'r') as file:
    for line in file:
        new_l = [i for i in line.split(' ') if len(i) > 0]
        mem1.append(convert(new_l, 9))
        cpu1.append(convert(new_l, 8))
        virt1.append(convert(new_l, 4))
        res1.append(convert(new_l, 5))

print(mem1)
print(cpu1)
fig, ax = plt.subplots()
ax.plot(mem1, label='MEM')
ax.plot(cpu1, label='CPU')

num_categories = 5
total_categories = len(mem1)
indices = [int(i) for i in range(0, total_categories, total_categories // num_categories)]
indices = indices[:-2]
indices.append(total_categories - 1)
desired_ticks = [mem1[i] for i in indices]
ax.yaxis.set_major_locator(ticker.FixedLocator(desired_ticks))
ax.legend()

fig.savefig('mem_cpu.png')

print()
print()

print(virt1)
print(res1)
fig, ax = plt.subplots()
ax.plot(virt1, label='VIRT')
ax.plot(res1, label='RES')

num_categories = 5
total_categories = len(virt1)
indices = [int(i) for i in range(0, total_categories, total_categories // num_categories)]
indices = indices[:-1]
indices.append(total_categories - 1)
desired_ticks = [virt1[i] for i in indices]
ax.yaxis.set_major_locator(ticker.FixedLocator(desired_ticks))
ax.legend()

fig.savefig('virt_res.png')

mem = []
swap = []
with open('mem.log', 'r') as file:
    for line in file:
        mem.append(int(line))

with open('swap.log', 'r') as file:
    for line in file:
        swap.append(int(line))

print()
print()

print(mem)
print(swap)
fig, ax = plt.subplots()
ax.plot(mem, label='RAM')
ax.plot(swap, label='SWAP')

num_categories = 5
total_categories = len(virt1)
indices = [int(i) for i in range(0, total_categories, total_categories // num_categories)]
indices = indices[:-2]
indices.append(total_categories - 1)
desired_ticks = [mem[i] for i in indices]
ax.yaxis.set_major_locator(ticker.FixedLocator(desired_ticks))
ax.legend()

fig.savefig('ram_swap.png')

#2

mem1 = []
cpu1 = []
virt1 = []
res1 = []

with open('params1_2.log', 'r') as file:
    for line in file:
        new_l = [i for i in line.split(' ') if len(i) > 0]
        mem1.append(convert(new_l, 9))
        cpu1.append(convert(new_l, 8))
        virt1.append(convert(new_l, 4))
        res1.append(convert(new_l, 5))

mem2 = []
cpu2 = []
virt2 = []
res2 = []

with open('params2_2.log', 'r') as file:
    for line in file:
        new_l = [i for i in line.split(' ') if len(i) > 0]
        mem2.append(convert(new_l, 9))
        cpu2.append(convert(new_l, 8))
        virt2.append(convert(new_l, 4))
        res2.append(convert(new_l, 5))

print(mem1)
print(cpu1)
print(mem2)
print(cpu2)
fig, ax = plt.subplots()
ax.plot(mem1, label='MEM1')
ax.plot(cpu1, label='CPU1')
ax.plot(mem2, label='MEM2')
ax.plot(cpu2, label='CPU2')

num_categories = 5
total_categories = len(mem1)
indices = [int(i) for i in range(0, total_categories, total_categories // num_categories)]
indices = indices[:-2]
indices.append(total_categories - 1)
desired_ticks = [mem1[i] for i in indices]
ax.yaxis.set_major_locator(ticker.FixedLocator(desired_ticks))
ax.legend()

fig.savefig('mem_cpu_2.png')

print()
print()

print(virt1)
print(res1)
print(virt2)
print(res2)
fig, ax = plt.subplots()
ax.plot(virt1, label='VIRT1')
ax.plot(res1, label='RES1')
ax.plot(virt2, label='VIRT2')
ax.plot(res2, label='RES2')

num_categories = 5
total_categories = len(virt1)
indices = [int(i) for i in range(0, total_categories, total_categories // num_categories)]
indices = indices[:-1]
indices.append(total_categories - 1)
desired_ticks = [virt1[i] for i in indices]
ax.yaxis.set_major_locator(ticker.FixedLocator(desired_ticks))
ax.legend()

fig.savefig('virt_res_2.png')

mem = []
swap = []
with open('mem_2.log', 'r') as file:
    for line in file:
        mem.append(int(line))

with open('swap_2.log', 'r') as file:
    for line in file:
        swap.append(int(line))

print()
print()

print(mem)
print(swap)
fig, ax = plt.subplots()
ax.plot(mem, label='RAM')
ax.plot(swap, label='SWAP')

num_categories = 5
total_categories = len(virt1)
indices = [int(i) for i in range(0, total_categories, total_categories // num_categories)]
indices = indices[:-2]
indices.append(total_categories - 1)
desired_ticks = [mem[i] for i in indices]
ax.yaxis.set_major_locator(ticker.FixedLocator(desired_ticks))
ax.legend()

fig.savefig('ram_swap_2.png')