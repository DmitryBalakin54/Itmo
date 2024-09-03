comp_set = {}
global_index = 0
inp = ""


class B:
    def __init__(self):
        self.w = [0] * 7
        self.w[1] = 1

    get = (lambda self:  self.w)

    @staticmethod
    def parse():
        global global_index

        global_index += 1

        return B()


class L:
    def __init__(self, comb_obj):
        self.comp_obj = comb_obj
        self.w = [0] * 7
        self.is_cnt = False

    def get(self):
        if self.is_cnt:
            return self.w

        comb_val = self.comp_obj.get()
        self.w[0] = 1

        for n in range(6):
            for i in range(n + 1):
                self.w[n + 1] += comb_val[i + 1] * self.w[n - i]

        self.is_cnt = True

        return self.w

    @staticmethod
    def parse():
        global global_index

        global_index += 2
        comb_obj = parse()
        global_index += 1

        return L(comb_obj)


class S:
    def __init__(self, comb_obj):
        self.comb_obj = comb_obj
        self.w = [0] * 7
        self.is_cnt = False

    def get(self):
        if self.is_cnt:
            return self.w

        comb_val = self.comb_obj.get()
        arr = [[0] * 7 for _ in range(7)]

        for i in range(7):
            arr[0][i] = 1

        for n in range(6):
            for k in range(6):
                for i in range((n + 1) // (k + 1) + 1):
                    arr[n + 1][k + 1] += self.comp(max(comb_val[k + 1] + i - 1, 0), i) * arr[n + 1 - i * (k + 1)][k]

        for n in range(7):
            self.w[n] = arr[n][n]

        self.is_cnt = True

        return self.w

    @staticmethod
    def comp(n, k):
        if not (n in comp_set):
            comp_set[n] = {}

        if not (k in comp_set[n]):
            if not (k <= n):
                comp_set[n][k] = 0

                return comp_set[n][k]

            res = 1

            for i in range(k):
                res = (res * (n - i)) // (i + 1)
            comp_set[n][k] = res

            return res

        return comp_set[n][k]

    @staticmethod
    def parse():
        global global_index

        global_index += 2
        comb_obj = parse()
        global_index += 1

        return S(comb_obj)


class P:
    def __init__(self, first_obj, second_obj):
        self.first_obj = first_obj
        self.second_obj = second_obj
        self.w = [0] * 7
        self.is_cnt = False

    def get(self):
        if self.is_cnt:
            return self.w

        f_val = self.first_obj.get()
        s_val = self.second_obj.get()

        for n in range(1, 8):
            for i in range(n):
                self.w[n - 1] += f_val[i] * s_val[n - i - 1]

        self.is_cnt = True

        return self.w

    @staticmethod
    def parse():
        global global_index

        global_index += 2
        first_obj = parse()
        global_index += 1
        second_obj = parse()
        global_index += 1

        return P(first_obj, second_obj)


def parse():
    global global_index

    return globals()[inp[global_index]].parse()


def main():
    global inp
    inp = input()
    print(*parse().get())


main()
