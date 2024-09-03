import copy
import sys


def aaaaa():
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










kr = []

symbs = {0: '(',
         1: '->',
         2: ')',
         3: '->(',
         4: '))->',
         5: ')->',
         6: '!',
         7: '&',
         8: '|',
         9: '(!,',
         10: '(&,',
         11: '(|,',
         12: '(->,',
         13: '|-',
         14: '#',
         15: ','}


def parse(_ln):
    global ln, pos
    ln = _ln + symbs[14]
    pos = 0

    def skip(s):
        global ln, pos
        if ln.startswith(s, pos):
            pos += len(s)
            return True
        return False

    def e():
        x = dij()
        if skip(symbs[1]):
            x = symbs[12] + x + symbs[15] + e() + symbs[2]
        return x

    def dij():
        x = con()
        while skip(symbs[8]):
            x = symbs[11] + x + symbs[15] + con() + symbs[2]
        return x

    def con():
        x = nt()
        while skip(symbs[7]):
            x = symbs[10] + x + symbs[15] + nt() + symbs[2]
        return x

    def nt():
        global pos, ln
        if skip(symbs[0]):
            x = e()
            skip(symbs[2])
            return x
        if skip(symbs[6]):
            return symbs[9] + str(nt()) + symbs[2]
        x = ''
        while ln[pos].isdigit() or ln[pos].isalpha() or ln[pos] == chr(39):
            x += ln[pos]
            pos += 1
        return x

    return e()


axs = list(map(parse, [
    'A->B->A',
    '(A->B)->(A->B->C)->(A->C)',
    'A->B->A&B',
    'A&B->A',
    'A&B->B',
    'A->A|B',
    'B->A|B',
    '(A->C)->(B->C)->(A|B->C)',
    '(A->B)->(A->!B)->(!A)',
    '!!A->A'
]))


def get_rsns(sts):
    st_sets = {}
    st_second_sets = {}
    map_for_ded = {}
    for i in range(len(sts)):
        st = sts[i]
        if check_for_ax(st):
            pass
        elif check_for_hyp(st):
            pass
        elif check_for_mp(sts, i, st_sets, st_second_sets):
            pass
        elif check_for_ded(sts, i, map_for_ded):
            pass
        if st[2] not in map_for_ded:
            map_for_ded[st[2]] = i
        if st[3] not in st_sets:
            st_sets[st[3]] = {}
            st_second_sets[st[3]] = {}
        st_set = st_sets[st[3]]
        st_second_set = st_second_sets[st[3]]
        stC = st[1]
        if stC not in st_set:
            st_set[stC] = []
        st_set[stC].append(i)
        if stC[0] != symbs[0]:
            continue
        optor, opnd1, opnd2, _ = get_op_and_opnds(stC)
        if optor == symbs[1]:
            if opnd2 not in st_second_set:
                st_second_set[opnd2] = []
            st_second_set[opnd2].append((i, opnd1))


def get_op_and_opnds(st):
    op = st[1:st.find(symbs[15])]
    if op == symbs[6]:
        return op, st[st.find(symbs[15]) + 1: len(st) - 1], '', 1
    opnd1, cnt_commas, balance, ind = '', 0 , 0, st.find(symbs[15]) + 1
    while balance != 0 or cnt_commas == 0:
        sym = st[ind]
        opnd1 += sym
        if sym == symbs[0]:
            balance += 1
        elif sym == symbs[2]:
            balance -= 1
        elif sym == symbs[15]:
            cnt_commas += 1
        ind += 1
    if st[ind] != symbs[15]:
        ind -= 1
        opnd1 = opnd1[:len(opnd1) - 1]
    opnd2 = st[ind + 1:len(st) - 1]
    return op, opnd1, opnd2, 2


def get_ctx_and_st(ln):
    ctx, ctx_cnt, raw_ctx_cnt = {}, {}, {}
    pos, num = 0, 1
    while not ln.startswith(symbs[13], pos):
        raw_expr = ''
        while not ln.startswith(symbs[15], pos) and not ln.startswith(symbs[13], pos):
            raw_expr += ln[pos]
            pos += 1
        expr = parse(raw_expr)
        ctx[expr] = num
        if expr not in ctx_cnt:
            ctx_cnt[expr] = 0
            raw_ctx_cnt[raw_expr] = 0
        ctx_cnt[expr] = ctx_cnt[expr] + 1
        raw_ctx_cnt[raw_expr] = raw_ctx_cnt[raw_expr] + 1
        num += 1
        if ln.startswith(symbs[15], pos):
            pos += 1
    _st = parse(ln[pos + 2:])
    raw_ex, en_ctx, en_ctx_list, st = ln[pos + 2:], ctx_cnt.copy(), [], _st
    while True:
        if len(st) < 3 or st[1:3] != symbs[1]:
            en_ctx_list.append(normalize_expr(st))
            break
        _, opnd1, opnd2, _ = get_op_and_opnds(st)
        if opnd1 not in en_ctx:
            en_ctx[opnd1] = 0
        en_ctx[opnd1] = en_ctx[opnd1] + 1
        en_ctx_list.append(normalize_expr(opnd1))
        st = opnd2
    en_ctx = (tuple(sorted(en_ctx.items())), st)
    return {0: ctx, 1: _st,
            7: '[Incorrect]',
            2: en_ctx,
            3: tuple(sorted(ctx_cnt.items())),
            4: raw_ex, 5: raw_ctx_cnt,
            6: en_ctx_list}


def check_for_hyp(ln):
    st = ln[1]
    ctx = ln[0]
    if st in ctx:
        ln[7] = "[H. " + str(ctx[st]) + "]"
        return True
    return False


def eqs_st_to_ax(st, ax, elements=None):
    if elements is None:
        elements = {}
    if ax[0] != symbs[0]:
        if ax in elements:
            if st != elements[ax]:
                return False
            return True
        elements[ax] = st
        return True
    if st[0] == symbs[0] and ax[0] == symbs[0]:
        axOp, axOpnd1, axOpnd2, axNum = get_op_and_opnds(ax)
        axOpnds = [axOpnd1, axOpnd2]
        op, opnd1, opnd2, num = get_op_and_opnds(st)
        opnds = [opnd1, opnd2]
        if axOp != op:
            return False
        for i in range(axNum):
            axOpnd = axOpnds[i]
            opnd = opnds[i]
            b = eqs_st_to_ax(opnd, axOpnd, elements)
            if not b:
                return False
        return True
    return False


def check_for_ax(ln):
    st = ln[1]
    if st[0] != symbs[0]:
        return False
    for i in range(len(axs)):
        ax = axs[i]
        if eqs_st_to_ax(st, ax):
            ln[7] = '[A. s. ' + str(i + 1) + "]"
            return True
    return False


def check_for_mp(sts, index, st_sets, st_second_sets):
    st = sts[index][1]
    ctx = sts[index][3]
    if ctx in st_sets:
        st_set = st_sets[ctx]
        st_second_set = st_second_sets[ctx]
        if st in st_second_set:
            arr = st_second_set[st]
            for i, opnd in arr:
                if opnd in st_set:
                    num = st_set[opnd]
                    sts[index][7] = '[M. ' + str(num[0] + 1) + ', ' + str(i + 1) + "]"
                    return True
    return False


def check_for_ded(sts, index, map_for_ded):
    st = sts[index]
    if st[2] in map_for_ded:
        st[7] = '[D. ' + str(map_for_ded[st[2]] + 1) + "]"
        return True
    return False


def f1(new_prf, aph, n):
    new_prf.append(
        {4: aph + symbs[3] + aph + symbs[1] + aph + symbs[2],
         7: '[A. s. 1]'})
    new_prf.append(
        {4:
             symbs[0] + aph + symbs[3] + aph + symbs[1] + aph + symbs[4] +
             symbs[0] + aph + symbs[3] + aph + symbs[1] + aph + symbs[5] + aph + symbs[5] +
             symbs[0] + aph + symbs[1] + aph + symbs[2],
         7: '[A. s. 2]'})
    new_prf.append(
        {4:
             symbs[0] + aph + symbs[3] + aph + symbs[1] + aph + symbs[5] + aph + symbs[5] +
             symbs[0] + aph + symbs[1] + aph + symbs[2],
         7: '[M. ' + str(n + 1) + ', ' + str(n + 2) + ']'})
    new_prf.append(
        {4:
             aph + symbs[3] + aph + symbs[1] + aph + symbs[5] + aph,
         7: '[A. s. 1]'})
    new_prf.append(
        {4:
             aph + symbs[1] + aph,
         7: '[M. ' + str(n + 4) + ', ' + str(n + 3) + ']'
         })


def f2(new_prf, aph, n, dj, dn, old_to_new, k, j):
    new_prf.append(
        {4:
             symbs[0] + aph + symbs[1] + dj + symbs[5] +
             symbs[0] + aph + symbs[1] + dj + symbs[1] + dn + symbs[5] +
             symbs[0] + aph + symbs[1] + dn + symbs[2],
         7: '[A. s. 2]'
         }
    )
    new_prf.append(
        {4:
             symbs[0] + aph + symbs[1] + dj + symbs[1] + dn + symbs[5] +
             symbs[0] + aph + symbs[1] + dn + symbs[2],
         7: '[M. ' + str(old_to_new[j]) + ', ' + str(n + 1) + ']'
         }
    )
    new_prf.append(
        {4:
             aph + symbs[1] + dn,
         7: '[M. ' + str(old_to_new[k]) + ', ' + str(n + 2) + ']'
         }
    )


def f3(new_prf, aph, n, dn):
    new_prf.append(
        {4:
             dn + symbs[1] + aph + symbs[1] + dn,
         7: '[A. s. 1]'
         }
    )
    new_prf.append({4: dn, 7: '[A. or H.]'})
    new_prf.append({
        4: aph + symbs[1] + dn,
        7: '[M. ' + str(n + 2) + ', ' + str(n + 1) + ']'
    })


def delete_hyp(hyp, cur_prf):
    new_prf = []
    aph = hyp
    old_to_new = [-1 for _ in range(len(cur_prf))]
    for i in range(len(cur_prf)):
        st = cur_prf[i]
        dn = st[4]
        n = len(new_prf)
        if dn == aph:
            f1(new_prf, aph, n)
            old_to_new[i] = n + 5
        elif st[7][1] == 'M':
            rsn = st[7]
            j = int(rsn[rsn.find(' ') + 1:rsn.find(symbs[15])]) - 1
            k = int(rsn[rsn.find(symbs[15]) + 2:len(rsn) - 1]) - 1
            dj = cur_prf[j][4]
            f2(new_prf, aph, n, dj, dn, old_to_new, k, j)
            old_to_new[i] = n + 3
        else:
            f3(new_prf, aph, n, dn)
            old_to_new[i] = n + 3
    for i in range(len(new_prf)):
        new_prf[i][4] = normalize(new_prf[i][4])
    return new_prf


def add_hyp(hyp, rest, cur_prf):
    n = len(cur_prf)
    cur_prf.append(
        {4: hyp,
         7: '[H]'})
    cur_prf.append(
        {4: rest,
         7: '[M. ' + str(n + 1) + ', ' + str(n) + ']'})


def normalize(ln):
    return normalize_expr(parse(ln))


def normalize_expr(expr):
    _expr = expr
    if _expr[0] == symbs[0]:
        op, opnd1, opnd2, numb = get_op_and_opnds(_expr)
        if numb == 1:
            return symbs[6] + normalize_expr(opnd1)
        else:
            return symbs[0] + normalize_expr(opnd1) + op + normalize_expr(opnd2) + symbs[2]
    return _expr


def construct_ded_another(index_new, index_old, sts):
    st, prev_st = sts[index_new], sts[index_old]
    st_new, st_old = st[6], prev_st[6]
    cur_prf = get_prf_another(sts, index_old)
    i = len(st_new) - 2
    j = len(st_old) - 2
    while i >= 0 and j >= 0:
        if st_new[i] == st_old[j]:
            i -= 1
            j -= 1
        else:
            break
    for jj in range(j + 1):
        add_hyp(st_old[jj], symbs[1].join(st_old[jj + 1:]), cur_prf)
    for ii in range(i, -1, -1):
        cur_prf = delete_hyp(st_new[ii], cur_prf)
    return cur_prf


def make_num(rsn, i):
    num = ''
    while rsn[i].isnumeric():
        num += rsn[i]
        i += 1

    return int(num), i


def get_prf_another(sts, index):
    global kr
    if len(kr[index]) != 0:
        return copy.deepcopy(kr[index])
    st = sts[index]
    if (rsn := st[7])[1] == 'D':
        start, end = rsn.find('.'), len(rsn) - 1
        kr[index] = construct_ded_another(index, int(rsn[start + 2:end]) - 1, sts)
    elif rsn[1] == 'M':
        numbers = []
        i = 0
        while i < len(rsn):
            if not rsn[i].isnumeric():
                i += 1
            else:
                num = ''
                while rsn[i].isnumeric():
                    num += rsn[i]
                    i += 1
                numbers.append(int(num) - 1)
        prf1, prf2 = get_prf_another(sts, numbers[0]), get_prf_another(sts, numbers[1])
        for j in range(len(prf2)):
            rsn = prf2[j][7]
            if rsn[1] == 'M':
                numbers1 = []
                i = 0
                while i < len(rsn):
                    if rsn[i].isnumeric():
                        num, i = make_num(rsn, i)
                        numbers1.append(num)
                    else:
                        i += 1
                prf2[j][7] = '[M. ' + str(numbers1[0] + len(prf1)) + ', ' + \
                             str(numbers1[1] + len(prf1)) + ']'
        prf = []

        for p in prf1:
            prf.append(p)

        for p in prf2:
            prf.append(p)
        prf.append({
            4: normalize(st[4]),
            7: '[M. ' + str(len(prf1)) + ', ' + str(len(prf)) + ']'
        })
        kr[index] = prf
    else:
        kr[index] = [{
            4: normalize(st[4]),
            7: '[H. or A.]'
        }]
    return copy.deepcopy(kr[index])


sys.setrecursionlimit(2 ** 30)
sts = []
raw_sts = []
last_str = ''
for _ln in sys.stdin:
    ln = ''
    for x in _ln:
        if not x.isspace():
            ln += x
    last_str = ln
    sts.append(get_ctx_and_st(ln))
    raw_sts.append(ln)
sys.stdin.close()
get_rsns(sts)

kr = [[] for _ in range(len(sts))]
prf = get_prf_another(sts, len(sts) - 1)
print(last_str)

for i in range(len(prf)):
    print(prf[i][4])
