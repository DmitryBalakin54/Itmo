import math

class D:
    def __init__(self, w, h, suf):
        self.w = w
        self.h = h
        self.suf = suf

def id():
    global n, m, s, END
    n, m = map(int, input().split())
    END = m + 10
    s = list(map(int, input().split()))
    s.append(0)
    n += 1

n, m, f = 0, 0, 0
END = 256
MAX_N = 150000 + 50
s = [0] * MAX_N
suff = [0] * MAX_N
lcp = [0] * MAX_N
c = [0] * MAX_N
ct = [0] * MAX_N
st = [0] * MAX_N
pos = [0] * MAX_N

n, m = map(int, input().split())
END = m + 10
s = list(map(int, input().split()))
s.append(0)
n += 1


cnt = [0] * END
for i in range(n):
    cnt[s[i]] += 1

for i in range(1, END):
    cnt[i] += cnt[i - 1]

for i in range(n - 1, -1, -1):
    cnt[s[i]] -= 1
    suff[cnt[s[i]]] = i

count = 1
c[suff[0]] = 0

for i in range(1, n):
    if s[suff[i]] != s[suff[i - 1]]:
        count += 1
    c[suff[i]] = count - 1

for l in range(int(math.log2(n)) + 1):
    x = 1 << l
    for i in range(n):
        st[i] = suff[i] - x
        if st[i] < 0:
            st[i] += n

    cnt = [0] * count
    for i in range(n):
        cnt[c[st[i]]] += 1

    for i in range(1, count):
        cnt[i] += cnt[i - 1]

    for i in range(n - 1, -1, -1):
        cnt[c[st[i]]] -= 1
        suff[cnt[c[st[i]]]] = st[i]

    count = 1
    c[suff[0]] = 0

    for i in range(1, n):
        if c[suff[i]] != c[suff[i - 1]] or c[(suff[i] + x) % n] != c[(suff[i - 1] + x) % n]:
            count += 1

        ct[suff[i]] = count - 1

    c = ct

for i in range(n):
    pos[suff[i]] = i

k = 0
for i in range(n):
    if k > 0:
        k -= 1

    if pos[i] == n - 1:
        lcp[n - 1] = -1
        k = 0
    else:
        cur = suff[pos[i] + 1]
        while max(i + k, cur + k) < n and s[i + k] == s[cur + k]:
            k += 1

        lcp[pos[i]] = k


st = []
ah = n - 1
aw = 1
asuf = -1

for i in range(1, n):
    cw = 1
    while st and lcp[i] <= st[-1].h:
        t = st.pop()
        cw += t.w
        if cw * t.h > ah * aw:
            ah = t.h
            aw = cw
            asuf = t.suf

    if not st or lcp[i] > st[-1].h:
        t = D(w=cw, h=lcp[i], suf=i)
        st.append(t)

print(f"{ah * aw}")
print(f"{ah}")
if asuf != -1:
    for i in range(ah):
        print(f"{s[suff[asuf] + i]}", end=" ")
else:
    for i in range(n - 1):
        print(f"{s[i]}", end=" ")
