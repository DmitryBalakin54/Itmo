

f = open('codeforces.com.har', 'r', encoding='utf-8')

lines = f.readlines()

ind = -1
l_ind = 0
lst = []
for line in lines:
    if ind == l_ind:
        ll = line.strip()
        if ll.startswith('\"url'):
            l_ind = line.index('://')
            new_l = line[l_ind + 3::]
            print(new_l)
            r_ind = new_l.index('/')
            lst.append(new_l[:r_ind:])

    if "\"GET\"" in line:
        ind = l_ind + 1

    l_ind += 1


f.close()

lst.sort()
lst = set(lst)
print(lst)