import sys
from copy import deepcopy

cnt = [0]


def lst_hash(lst):
    hs = 0
    for ind, i in enumerate(lst):
        try:
            hs += (ind + 5) * hash(i)
        except Exception as e:
            hs += lst_hash(i)
    return hs


def lst_set_hash(lst):
    hs = 0
    for ind, i in enumerate(lst):
        try:
            hs += hash(i)
        except Exception as e:
            hs += lst_hash(i)
    return hs


class List:
    def __init__(self, initial_list=None):
        self.lst = initial_list if initial_list is not None else []
        self.hash = 0.1
        self.set_hash = 0.1
        self.st = set(self.lst)
        # self.id = cnt[0]
        # cnt[0] += 1

    def __contains__(self, item):
        return item in self.st

    def append(self, item):
        self.lst.append(item)
        self.st.add(item)

    def extend(self, iterable):
        self.lst.extend(iterable)

    def pop(self, index=None):
        return self.lst.pop(index)

    def remove(self, item):
        self.lst.remove(item)

    def __getitem__(self, index):
        return self.lst[index]

    def __setitem__(self, index, value):
        self.lst[index] = value

    def __len__(self):
        return len(self.lst)

    def __repr__(self):
        return repr(self.lst)

    def __str__(self):
        return self.__repr__()

    def __hash__(self):
        if self.hash == 0.1:
            self.hash = lst_hash(self.lst)
        return self.hash

    def get_set_hash(self):
        if self.set_hash == 0.1:
            self.set_hash = lst_set_hash(self.lst)
        return self.set_hash


def parse(_line):
    global line, pos
    line = _line + '#'
    pos = 0

    def skip(s):
        global line, pos
        if line.startswith(s, pos):
            pos += len(s)
            return True
        return False

    def e():
        x = dij()
        if skip('->'):
            x = List(['->', x, e()])
        return x

    def dij():
        x = con()
        while skip('|'):
            x = List(['|', x, con()])
        return x

    def con():
        x = nt()
        while skip('&'):
            x = List(['&', x, nt()])
        return x

    def nt():
        global pos, line
        if skip('('):
            x = e()
            skip(')')
            return x
        if skip('!'):
            return List(['!', nt()])
        x = ''
        while line[pos].isdigit() or line[pos].isalpha() or line[pos] == chr(39):
            x += line[pos]
            pos += 1
        return x

    return e()


def _make_expr_str(expr):
    res = ''
    for i in expr:
        if isinstance(i, List):
            if i[0] == '!':
                res += '(' + i[0] + _make_expr_str(List([i[1]])) + ')'
            else:
                res += '(' + i[0] + ',' + _make_expr_str(List([i[1]])) + ',' + _make_expr_str(List([i[2]])) + ')'
        else:
            res += i

    return res


def make_expr_str(expr):
    return _make_expr_str(List([expr]))


def _make_normal_expr_str(expr):
    res = ''
    for i in expr:
        if isinstance(i, List):
            if i[0] == '!':
                res += '(' + i[0] + _make_normal_expr_str(List([i[1]])) + ')'
            else:
                res += '(' + _make_normal_expr_str(List([i[1]])) + i[0] + _make_normal_expr_str(List([i[2]])) + ')'
        else:
            res += i

    return res


def make_normal_expr_str(expr):
    return _make_normal_expr_str(List([expr]))


def set_eq(obj1, obj2):
    return obj1.get_set_hash() == obj2.get_set_hash()


def eq(obj1, obj2):
    if isinstance(obj1, str) and isinstance(obj2, str):
        return obj1 == obj2

    if isinstance(obj1, List) and isinstance(obj2, List):
        if len(obj1) != len(obj2):
            return False
        if hash(obj1) != hash(obj2):
            return False

        try:
            if obj1[0] in {'->', '!', '&', '|'} and eq(obj1[0], obj2[0]):
                return set_eq(obj1, obj2)
        except Exception as e:
            pass

        return True


    else:
        return False


def in_lst(el, lst):
    for e in lst:
        if eq(e, el):
            return True
    return False


class Expr:
    def __init__(self, l: str, r: str):
        self.l = List([parse(i) for i in l.split(',')] if l else [])
        self.r = parse(r)
        self.hash = 0.1
        self.id = cnt[0]
        cnt[0] += 1

    def __repr__(self):
        return ','.join(make_normal_expr_str(i) for i in self.l) + '|-' + make_normal_expr_str(self.r)

    def __hash__(self):
        if self.hash == 0.1:
            self.hash = hash((self.l, self.r))
        return self.hash

    def __eq__(self, other):
        if isinstance(other, Expr):
            return hash(self) == hash(other)
        return False


def check_ax(cur_expr, lines, ind):
    t = cur_expr.r
    try:
        if t[0] == t[2][0] == '->' and eq(t[1], t[2][2]):
            return '[Ax. sch. 1]'
    except Exception as e:
        pass

    try:
        if (t[0] == t[2][0] == t[2][2][0] == t[2][1][0] == t[2][1][2][0] == t[1][0] == '->' and
                eq(t[1][1], t[2][1][1]) and eq(t[1][1], t[2][2][1]) and
                eq(t[1][2], t[2][1][2][1]) and eq(t[2][1][2][2], t[2][2][2])):
            return '[Ax. sch. 2]'
    except Exception as e:
        pass

    try:
        if (t[0] == t[2][0] == '->' and t[2][2][0] == '&' and
            eq(t[1], t[2][2][1])) and eq(t[2][1], t[2][2][2]):
            return '[Ax. sch. 3]'
    except Exception as e:
        pass

    try:
        if t[0] == '->' and t[1][0] == '&':
            if eq(t[1][1], t[2]):
                return '[Ax. sch. 4]'
            elif eq(t[1][2], t[2]):
                return '[Ax. sch. 5]'
    except Exception as e:
        pass

    try:
        if t[0] == '->' and t[2][0] == '|':
            if eq(t[2][1], t[1]):
                return '[Ax. sch. 6]'
            elif eq(t[2][2], t[1]):
                return '[Ax. sch. 7]'
    except Exception as e:
        pass

    try:
        if (t[0] == t[1][0] == t[2][0] == t[2][1][0] == t[2][2][0] == '->' and
                t[2][2][1][0] == '|' and eq(t[1][1], t[2][2][1][1]) and eq(t[2][1][1], t[2][2][1][2]) and
                eq(t[1][2], t[2][1][2]) and eq(t[1][2], t[2][2][2])):
            return '[Ax. sch. 8]'
    except Exception as e:
        pass

    try:
        if (t[0] == t[1][0] == t[2][0] == t[2][1][0] == '->' and t[2][1][2][0] == t[2][2][0] == '!' and
                eq(t[1][1], t[2][1][1]) and eq(t[1][1], t[2][2][1]) and eq(t[1][2], t[2][1][2][1])):
            return '[Ax. sch. 9]'
    except Exception as e:
        pass

    try:
        if t[0] == '->' and t[1][0] == t[1][1][0] == '!' and eq(t[2], t[1][1][1]):
            return '[Ax. sch. 10]'
    except Exception as e:
        pass

    return None


mp_map = {}


def check_mp(cur_expr, lines, ind):
    l = len(cur_expr.l)
    mp = mp_map[l]
    l = len(ded_map[cur_expr.id][0].l)
    try:
        mp1 = ded_map_len[l + 1]
    except Exception as e:
        return None
    for i in mp.values():
        for j in mp1.values():
            fst, fi = lines[i[1]], i[1]
            snd, si = lines[j[1]], j[1]
            if len(snd.l) != len(fst.l) or len(snd.l) != len(cur_expr.l):
                continue
            try:
                if snd.r[0] == '->' and eq(fst.r, snd.r[1]) and eq(snd.r[2], cur_expr.r):
                    if not set_eq(snd.l, fst.l) or not set_eq(fst.l, cur_expr.l):
                        continue
                    return f'[M.P. {fi + 1}, {si + 1}]'
            except Exception as e:
                pass
    return None


# def check_mp(cur_expr, lines, ind):
#     for i in range(ind - 1):
#         for j in range(i + 1, ind):
#             fst = lines[i]
#             snd = lines[j]
#             if len(snd.l) != len(fst.l) or len(snd.l) != len(cur_expr.l):
#                 continue
#             try:
#                 if snd.r[0] == '->' and eq(fst.r, snd.r[1]) and eq(snd.r[2], cur_expr.r):
#                     if not set_eq(snd.l, fst.l) or not set_eq(fst.l, cur_expr.l):
#                         continue
#                     return f'[M.P. {i + 1}, {j + 1}]'
#             except Exception as e:
#                 pass
#
#             try:
#                 if fst.r[0] == '->' and eq(snd.r, fst.r[1]) and eq(fst.r[2], cur_expr.r):
#                     if not set_eq(fst.l, snd.l) or not set_eq(snd.l, cur_expr.l):
#                         continue
#                     return f'[M.P. {j + 1}, {i + 1}]'
#             except Exception as e:
#                 pass
#     return None


def make_empty_r(expr):
    i = expr.r
    new_l = List(deepcopy(expr.l))
    while isinstance(i, List):
        if i[0] == '->':
            new_l.append(i[1])
            i = i[2]
        else:
            break
    ex = Expr('', '')
    ex.l = new_l
    ex.r = i
    return ex


ded_map_len = {}
ded_map = {}


def check_ded(cur_expr, lines, ind):
    cur_ex, cur_num = ded_map[cur_expr.id]
    for i in ded_map_len[len(cur_ex.l)].values():
        new_ex, num = i
        if num == cur_num:
            continue
        if set_eq(cur_ex.l, new_ex.l) and eq(cur_ex.r, new_ex.r):
            return f'[Ded. {num + 1}]'

        # try:
        #     if in_lst(cur_expr.r[1], l) and eq(cur_expr.r[2], r) and cur_expr.r[0] == '->':
        #         return f'[Ded. {i + 1}]'
        # except Exception as e:
        #     pass
        #
        # try:
        #     if in_lst(r[1], cur_expr.l) and eq(r[2], cur_expr.r) and r[0] == '->':
        #         return f'[Ded. {i + 1}]'
        # except Exception as e:
        #     pass

    return None


def check_hyp(cur_expr, lines, ind):
    el = cur_expr.r
    lst = cur_expr.l
    for ind, e in enumerate(lst):
        if eq(e, el):
            return f'[Hyp. {ind + 1}]'
    return None


def check(lines, index):
    cur_expr = lines[index]
    for op in (check_ax, check_hyp, check_mp, check_ded):
        res = op(cur_expr, lines, index)
        if res:
            return res
    return '[Incorrect]'


def main():
    sys.setrecursionlimit(2 ** 30)
    parsed_lines = List([])
    ind = 0
    a = False
    if a:
        while True:
            ind += 1
            ln = ''.join(i for i in sys.stdin.readline() if not i.isspace())
            left, right = ln.split('|-')

            parsed_lines.append(Expr(left, right))
            # print(f'{parsed_lines[-1].l}  {parsed_lines[-1].r}')

            l = len(parsed_lines[-1].l)
            if l not in mp_map:
                mp_map[l] = {}
            mp_map[l][parsed_lines[-1].id] = (parsed_lines[-1], ind - 1)

            cur_ex = make_empty_r(parsed_lines[-1])
            l = len(cur_ex.l)
            if l not in ded_map_len:
                ded_map_len[l] = {}
            ded_map_len[l][parsed_lines[-1].id] = (cur_ex, ind - 1)
            ded_map[parsed_lines[-1].id] = (cur_ex, ind - 1)

            print(f'[{ind}] {parsed_lines[-1]} {check(parsed_lines, ind - 1)}')
    else:
        for s in sys.stdin.readlines():
            ind += 1
            ln = ''.join(i for i in s if not i.isspace())
            left, right = ln.split('|-')
            parsed_lines.append(Expr(left, right))
            # print(f'{parsed_lines[-1].l}  {parsed_lines[-1].r}')

            l = len(parsed_lines[-1].l)
            if l not in mp_map:
                mp_map[l] = {}
            mp_map[l][parsed_lines[-1].id] = (parsed_lines[-1], ind - 1)

            cur_ex = make_empty_r(parsed_lines[-1])
            l = len(cur_ex.l)
            if l not in ded_map_len:
                ded_map_len[l] = {}
            ded_map_len[l][parsed_lines[-1].id] = (cur_ex, ind - 1)
            ded_map[parsed_lines[-1].id] = (cur_ex, ind - 1)

            print(f'[{ind}] {ln} {check(parsed_lines, ind - 1)}')


if __name__ == '__main__':
    main()
