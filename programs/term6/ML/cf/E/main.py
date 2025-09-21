import math


class Matrix:
    def __init__(self, source):
        self.source = source
        self.rows = len(source)
        self.cols = len(source[0]) if self.rows > 0 else 0

    def map_elem(self, f):
        return Matrix([[f(x) for x in row] for row in self.source])

    def map_elem_indexed(self, f):
        return Matrix([[f(i, j, x) for j, x in enumerate(row)] for i, row in enumerate(self.source)])

    def __mul__(self, other):
        return self.multiply(other)

    def multiply(self, other, transpose=False, transpose_other=False):
        m = self.cols if not transpose else self.rows
        m1 = other.rows if not transpose_other else other.cols

        n = self.rows if not transpose else self.cols
        k = other.cols if not transpose_other else other.rows

        res = [[0.0 for _ in range(k)] for _ in range(n)]

        for i in range(n):
            for j in range(k):
                for l in range(m):
                    left = self.source[i][l] if not transpose else self.source[l][i]
                    right = other.source[l][j] if not transpose_other else other.source[j][l]
                    res[i][j] += left * right
        return Matrix(res)

    def __getitem__(self, idx):
        return self.source[idx]

    def __add__(self, other):
        return self.map_elem_indexed(lambda i, j, x: x + other[i][j])

    def hadamard(self, other):
        return self.map_elem_indexed(lambda i, j, x: x * other[i][j])

    def transpose(self):
        return Matrix([[self.source[j][i] for j in range(self.rows)] for i in range(self.cols)])

    def print(self):
        for row in self.source:
            print(' '.join(f"{x:.12f}" for x in row))

    @staticmethod
    def parse(rows):
        return Matrix([[float(x) for x in input().split()] for _ in range(rows)])

    @staticmethod
    def value(rows, cols, val):
        return Matrix([[val for _ in range(cols)] for _ in range(rows)])


class Node:
    def __init__(self):
        self.function_cache = None
        self.deriv = None
        self.edges = []

    def calc_function(self):
        if self.function_cache is None:
            self.function_cache = self.calc_function_inner()
        return self.function_cache

    def calc_function_inner(self):
        raise NotImplementedError

    def push_deriv(self):
        raise NotImplementedError

    def add_deriv(self, deriv):
        if self.deriv is None:
            self.deriv = deriv
        else:
            self.deriv = self.deriv + deriv

    def zero_matrix(self):
        f = self.calc_function()
        return Matrix.value(f.rows, f.cols, 0.0)

    def get_deriv(self):
        if self.deriv is None:
            return self.zero_matrix()
        return self.deriv


class VarNode(Node):
    def __init__(self, source):
        super().__init__()
        self.source = source

    def calc_function_inner(self):
        return self.source

    def push_deriv(self):
        pass


class TnhNode(Node):
    def __init__(self, arg):
        super().__init__()
        self.arg = arg
        self.edges = [arg]

    def calc_function_inner(self):
        return self.arg.calc_function().map_elem(math.tanh)

    def push_deriv(self):
        tanh = self.calc_function()
        deriv = tanh.map_elem_indexed(lambda i, j, x: (1.0 - x * x) * self.get_deriv()[i][j])
        self.arg.add_deriv(deriv)


class SigmNode(Node):
    def __init__(self, arg):
        super().__init__()
        self.arg = arg
        self.edges = [arg]

    def calc_function_inner(self):
        return self.arg.calc_function().map_elem(lambda x: 1.0 / (1.0 + math.exp(-x)))

    def push_deriv(self):
        sig = self.calc_function()
        deriv = sig.map_elem_indexed(lambda i, j, x: self.get_deriv()[i][j] * x * (1 - x))
        self.arg.add_deriv(deriv)


class MulNode(Node):
    def __init__(self, left, right):
        super().__init__()
        self.left = left
        self.right = right
        self.edges = [left, right]

    def calc_function_inner(self):
        return self.left.calc_function() * self.right.calc_function()

    def push_deriv(self):
        self.left.add_deriv(self.get_deriv().multiply(self.right.calc_function(), transpose_other=True))
        self.right.add_deriv(self.left.calc_function().multiply(self.get_deriv(), transpose=True))


class SumNode(Node):
    def __init__(self, *args):
        super().__init__()
        self.args = list(args)
        self.edges = self.args

    def calc_function_inner(self):
        return sum((arg.calc_function() for arg in self.args[1:]), self.args[0].calc_function())

    def push_deriv(self):
        for arg in self.args:
            arg.add_deriv(self.get_deriv())


class HadNode(Node):
    def __init__(self, *args):
        super().__init__()
        self.args = list(args)
        self.edges = self.args

    def calc_function_inner(self):
        result = self.args[0].calc_function()
        for arg in self.args[1:]:
            result = result.hadamard(arg.calc_function())
        return result

    def push_deriv(self):
        ex = self.args[0].calc_function()
        for i, node in enumerate(self.args):
            product = Matrix.value(ex.rows, ex.cols, 1.0)
            for j, arg in enumerate(self.args):
                if j != i:
                    product = product.hadamard(arg.calc_function())
            node.add_deriv(product.hadamard(self.get_deriv()))


def read_nodes():
    input_len = int(input())
    matrices = []
    for _ in range(12):
        if len(matrices) % 3 == 2:
            mat = Matrix.parse(1).transpose()
        else:
            mat = Matrix.parse(input_len)
        matrices.append(VarNode(mat))

    wF, uF, bF, wI, uI, bI, wO, uO, bO, wC, uC, bC = matrices
    seq_len = int(input())

    h0 = VarNode(Matrix.parse(1).transpose())
    c0 = VarNode(Matrix.parse(1).transpose())
    xs = []
    os = []
    h_prev = h0
    c_prev = c0

    for _ in range(seq_len):
        xi = VarNode(Matrix.parse(1).transpose())
        xs.append(xi)

        forget_gate = SigmNode(SumNode(MulNode(wF, xi), MulNode(uF, h_prev), bF))
        input_gate = SigmNode(SumNode(MulNode(wI, xi), MulNode(uI, h_prev), bI))
        new_state_gate = TnhNode(SumNode(MulNode(wC, xi), MulNode(uC, h_prev), bC))
        input_state = HadNode(input_gate, new_state_gate)
        c_curr = SumNode(HadNode(forget_gate, c_prev), input_state)
        output_gate = SigmNode(SumNode(MulNode(wO, xi), MulNode(uO, h_prev), bO))
        h_curr = HadNode(output_gate, c_curr)

        os.append(output_gate)
        h_prev = h_curr
        c_prev = c_curr

    for node in reversed(os + [h_prev, c_prev]):
        node.add_deriv(Matrix.parse(1).transpose())

    return matrices, h_prev, c_prev, h0, c0, xs, os


def topological_sort(start_nodes):
    visited = set()
    order = []

    def dfs(node):
        if node in visited:
            return
        visited.add(node)
        for edge in node.edges:
            dfs(edge)
        order.append(node)

    for node in start_nodes:
        dfs(node)
    return reversed(order)


wub_nodes, hm, cm, h0, c0, xs, os = read_nodes()
for o in os:
    o.calc_function().print()
hm.calc_function().print()
cm.calc_function().print()

for node in topological_sort([hm]):
    node.push_deriv()

for x in reversed(xs):
    x.get_deriv().print()
h0.get_deriv().print()
c0.get_deriv().print()
for node in wub_nodes:
    node.get_deriv().print()
