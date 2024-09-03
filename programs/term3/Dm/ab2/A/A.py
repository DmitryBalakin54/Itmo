import bisect

with open('schedule.in', 'r') as inf:
    with open('schedule.out', 'w') as outf:

        n = int(inf.readline())
        tasks = {}
        for i in range(n):
            a, b = map(int, inf.readline().split())
            if a not in tasks:
                tasks[a] = []
            tasks[a].append(b)

        res = 0
        cur_tasks = []

        for time, values in sorted(tasks.items()):
            for t in values:
                bisect.insort(cur_tasks, t)
            while len(cur_tasks) > time:
                res += cur_tasks.pop(0)

        outf.write(str(res))