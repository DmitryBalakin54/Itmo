import sys


def bin_search(lst: list[int], val: int) -> int:
    l = -1
    r = len(lst)
    while l < r - 1:
        m = (l + r) // 2
        sys.stdout.write(f'{1} {lst[m]} {val}\n')
        sys.stdout.flush()

        s = sys.stdin.readline()
        if s == 'YES\n':
            l = m
        else:
            r = m
    return r


n = int(sys.stdin.readline())
lst = [1]

for i in range(2, n + 1):
    lst.insert(bin_search(lst, i), i)

res = '0 ' + ' '.join(str(i) for i in lst) + ' \n'
sys.stdout.write(res)
