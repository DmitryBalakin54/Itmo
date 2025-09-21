import sys
from typing import List, Callable


class SquareMatrix:
    def __init__(self, source: List[List[float]]):
        self.dim = len(source)
        self.source = [row.copy() for row in source]
        assert len(source) == len(source[0]), "wrong"

    def map_elem(self, f: Callable[[float], float]) -> 'SquareMatrix':
        return SquareMatrix([[f(elem) for elem in row] for row in self.source])

    def map_elem_indexed(self, f: Callable[[int, int, float], float]) -> 'SquareMatrix':
        return SquareMatrix([
            [f(row_idx, col_idx, elem) for col_idx, elem in enumerate(row)]
            for row_idx, row in enumerate(self.source)
        ])

    def __mul__(self, another: 'SquareMatrix') -> 'SquareMatrix':
        return self.times(another)

    def times(self, another: 'SquareMatrix', transpose: bool = False,
              transpose_another: bool = False) -> 'SquareMatrix':
        assert self.dim == another.dim, f"wrong"

        res_matrix = SquareMatrix.get_value_matrix(self.dim, 0.0)
        for i in range(self.dim):
            for j in range(self.dim):
                for l in range(self.dim):
                    left = self.source[l][i] if transpose else self.source[i][l]
                    right = another.source[j][l] if transpose_another else another.source[l][j]
                    res_matrix.source[i][j] += left * right
        return res_matrix

    def __getitem__(self, ind: int) -> List[float]:
        return self.source[ind]

    def __add__(self, another: 'SquareMatrix') -> 'SquareMatrix':
        another_matrix = another.source
        return self.map_elem_indexed(lambda row, col, value: value + another_matrix[row][col])

    def print(self):
        for row in self.source:
            print(" ".join(f"{elem}" for elem in row), end= " ")

    @staticmethod
    def parse_matrix(dim: int, source: List[float]) -> 'SquareMatrix':
        matrix = []
        for i in range(dim):
            start = dim * i
            end = dim * (i + 1)
            matrix.append(source[start:end].copy())
        return SquareMatrix(matrix)

    @staticmethod
    def get_value_matrix(dim: int, value: float) -> 'SquareMatrix':
        return SquareMatrix([[value for _ in range(dim)] for _ in range(dim)])


class Matrix3D:
    def __init__(self, layers: List[SquareMatrix]):
        self.depth = len(layers)
        self.dim = layers[0].dim
        self.layers = layers
        self.indices = range(self.depth)

    def __getitem__(self, ind: int) -> SquareMatrix:
        return self.layers[ind]

    def __add__(self, other: 'Matrix3D') -> 'Matrix3D':
        return Matrix3D([layer + other[i] for i, layer in enumerate(self.layers)])

    def map(self, f: Callable[[SquareMatrix], SquareMatrix]) -> 'Matrix3D':
        return Matrix3D([f(layer) for layer in self.layers])

    def map_indexed(self, f: Callable[[int, SquareMatrix], SquareMatrix]) -> 'Matrix3D':
        return Matrix3D([f(i, layer) for i, layer in enumerate(self.layers)])

    @staticmethod
    def parse_matrix_3d(depth: int, dim: int, source: List[float]) -> 'Matrix3D':
        matrix = []
        dim_sqr = dim * dim
        for i in range(depth):
            start = dim_sqr * i
            end = dim_sqr * (i + 1)
            matrix.append(SquareMatrix.parse_matrix(dim, source[start:end]))
        return Matrix3D(matrix)

    @staticmethod
    def get_value_matrix_3d(depth: int, dim: int, value: float) -> 'Matrix3D':
        return Matrix3D([SquareMatrix.get_value_matrix(dim, value) for _ in range(depth)])


class Node:
    def __init__(self):
        self.function_cache = None
        self.my_deriv = None

    def calc_function_inner(self) -> Matrix3D:
        raise NotImplementedError()

    def push_deriv(self):
        raise NotImplementedError()

    def calc_function(self) -> Matrix3D:
        if self.function_cache is None:
            self.function_cache = self.calc_function_inner()
        return self.function_cache

    @property
    def deriv(self) -> Matrix3D:
        if not hasattr(self, 'my_deriv') or self.my_deriv is None:
            func = self.calc_function()
            return Matrix3D.get_value_matrix_3d(func.depth, func.dim, 0.0)
        return self.my_deriv

    def add_deriv(self, deriv: Matrix3D):
        if not hasattr(self, 'my_deriv') or self.my_deriv is None:
            self.my_deriv = deriv
        else:
            self.my_deriv = self.my_deriv + deriv

    @staticmethod
    def read_nodes() -> List['Node']:
        input_matrix_info = sys.stdin.readline().split()
        dim = int(input_matrix_info[0])
        depth = int(input_matrix_info[1])
        input_matrix_3d = Matrix3D.parse_matrix_3d(depth, dim, [float(x) for x in input_matrix_info[2:]])

        res = [VarNode(input_matrix_3d)]
        l = int(sys.stdin.readline())

        for _ in range(l):
            data = sys.stdin.readline().split()
            args = data[1:]

            if data[0] == "relu":
                res.append(ReluNode(1.0 / int(args[0]), res[-1]))
            elif data[0] == "pool":
                res.append(PoolNode(int(args[0]), res[-1]))
            elif data[0] == "bias":
                res.append(BiasNode([float(x) for x in args], res[-1]))
            else:
                h, k, s, p = map(int, args[:4])
                kernel = [float(x) for x in args[4:]]

                if data[0] == "cnvm":
                    res.append(CnvmNode(h, k, s, p, kernel, res[-1]))
                elif data[0] == "cnve":
                    res.append(CnveNode(h, k, s, p, kernel, res[-1]))
                elif data[0] == "cnvc":
                    res.append(CnvcNode(h, k, s, p, kernel, res[-1]))

        return res


class VarNode(Node):
    def __init__(self, layers: Matrix3D):
        super().__init__()
        self.layers = layers

    def calc_function_inner(self) -> Matrix3D:
        return self.layers

    def push_deriv(self):
        pass


class ReluNode(Node):
    def __init__(self, alpha: float, prev: Node):
        super().__init__()
        self.alpha = alpha
        self.prev = prev

    def calc_function_inner(self) -> Matrix3D:
        return self.prev.calc_function().map(
            lambda layer: layer.map_elem(lambda x: max(x, self.alpha * x)))

    def push_deriv(self):
        prev_func = self.prev.calc_function()
        self.prev.add_deriv(prev_func.map_indexed(
            lambda layer_idx, layer: layer.map_elem_indexed(
                lambda row, col, x: self.deriv[layer_idx][row][col] * (self.alpha if x < 0.0 else 1.0))))


class PoolNode(Node):
    def __init__(self, sub: int, prev: Node):
        super().__init__()
        self.sub = sub
        self.prev = prev

    def calc_function_inner(self) -> Matrix3D:
        return self.prev.calc_function().map(self._pool_layer)

    def _pool_layer(self, layer: SquareMatrix) -> SquareMatrix:
        new_dim = layer.dim // self.sub
        res_matrix = []

        for i_iter in range(new_dim):
            row = []
            for j_iter in range(new_dim):
                i_st = i_iter * self.sub
                j_st = j_iter * self.sub
                mx_value = None

                for i in range(self.sub):
                    for j in range(self.sub):
                        cell_val = layer[i_st + i][j_st + j]
                        mx_value = max(mx_value or cell_val, cell_val)

                row.append(mx_value)
            res_matrix.append(row)

        return SquareMatrix(res_matrix)

    def push_deriv(self):
        layers = self.calc_function()
        prev_layers = self.prev.calc_function()

        deriv = prev_layers.map_indexed(lambda layer_idx, prev_layer:
                                        self._push_deriv_layer(layer_idx, prev_layer, layers[layer_idx]))

        self.prev.add_deriv(deriv)

    def _push_deriv_layer(self, layer_idx: int, prev_layer: SquareMatrix, layer: SquareMatrix) -> SquareMatrix:
        prev_layer_deriv = SquareMatrix.get_value_matrix(prev_layer.dim, 0.0)
        layer_deriv = self.deriv[layer_idx]
        dim = layer.dim

        for i_iter in range(dim):
            for j_iter in range(dim):
                mx_value = layer[i_iter][j_iter]
                i_st = i_iter * self.sub
                j_st = j_iter * self.sub

                for i in range(self.sub):
                    for j in range(self.sub):
                        cell_val = prev_layer[i_st + i][j_st + j]
                        if cell_val == mx_value:
                            prev_layer_deriv[i_st + i][j_st + j] = layer_deriv[i_iter][j_iter]

        return prev_layer_deriv


class BiasNode(Node):
    def __init__(self, b: List[float], prev: Node):
        super().__init__()
        self.b = b
        self.prev = prev

    def calc_function_inner(self) -> Matrix3D:
        return self.prev.calc_function().map_indexed(
            lambda layer_idx, layer: layer.map_elem(lambda x: x + self.b[layer_idx]))

    def push_deriv(self):
        self.prev.add_deriv(self.deriv)

    def print_param_deriv(self):
        sums = []
        for layer in self.deriv.layers:
            total = 0.0
            for row in layer.source:
                total += sum(row)
            sums.append(total)
        print(" ".join(f"{s}" for s in sums), end=" ")


class CnvxNode(Node):
    def __init__(self, h: int, k: int, s: int, p: int, unparsed_kernel: List[float], prev: Node):
        super().__init__()
        self.h = h
        self.k = k
        self.s = s
        self.p = p
        self.unparsed_kernel = unparsed_kernel
        self.prev = prev
        self._kernel = None
        self._kernel_deriv = None
        self._padded_layers_cache = None

    def fill(self, dim: int, matrix: SquareMatrix):
        raise NotImplementedError()

    @property
    def kernel(self) -> List[Matrix3D]:
        if self._kernel is None:
            depth = self.prev.calc_function().depth
            matrix_elem_cnt = depth * self.k * self.k
            self._kernel = []

            for layer_ind in range(self.h):
                start = matrix_elem_cnt * layer_ind
                end = matrix_elem_cnt * (layer_ind + 1)
                self._kernel.append(Matrix3D.parse_matrix_3d(
                    depth, self.k, self.unparsed_kernel[start:end]))

        return self._kernel

    @property
    def kernel_deriv(self) -> List[Matrix3D]:
        if self._kernel_deriv is None:
            depth = self.kernel[0].depth
            dim = self.kernel[0].dim
            self._kernel_deriv = [Matrix3D.get_value_matrix_3d(depth, dim, 0.0) for _ in range(self.h)]

        return self._kernel_deriv

    def get_padded_matrix(self, layer: SquareMatrix) -> SquareMatrix:
        dim = layer.dim
        padded = []

        for _ in range(self.p):
            padded.append([0.0] * (dim + 2 * self.p))

        for i in range(dim):
            row = [0.0] * self.p + layer[i].copy() + [0.0] * self.p
            padded.append(row)

        for _ in range(self.p):
            padded.append([0.0] * (dim + 2 * self.p))

        matrix = SquareMatrix(padded)
        self.fill(dim, matrix)
        return matrix

    @property
    def padded_layers(self) -> Matrix3D:
        if self._padded_layers_cache is None:
            self._padded_layers_cache = self.prev.calc_function().map(self.get_padded_matrix)
        return self._padded_layers_cache

    def calc_function_inner(self) -> Matrix3D:
        padded_layers = self.padded_layers
        old_layers_depth = padded_layers.depth
        dim = padded_layers.dim
        new_dim = (dim - self.k) // self.s + 1
        new_layers = []

        for layer_ind in range(self.h):
            new_layer = []

            for i_iter in range(new_dim):
                row = []
                for j_iter in range(new_dim):
                    cell_value = 0.0
                    i_st = i_iter * self.s
                    j_st = j_iter * self.s

                    for old_layer_ind in range(old_layers_depth):
                        for i in range(self.k):
                            for j in range(self.k):
                                cell_value += (
                                        padded_layers[old_layer_ind][i_st + i][j_st + j] *
                                        self.kernel[layer_ind][old_layer_ind][i][j]
                                )
                    row.append(cell_value)
                new_layer.append(row)

            new_layers.append(SquareMatrix(new_layer))

        return Matrix3D(new_layers)

    def push_deriv(self):
        padded_layers = self.padded_layers
        old_layers = self.prev.calc_function()
        old_dim = old_layers.dim
        padded_dim = old_dim + 2 * self.p
        push_deriv = Matrix3D.get_value_matrix_3d(old_layers.depth, padded_dim, 0.0)
        new_dim = self.deriv.dim

        for layer_ind in range(self.h):
            for i_iter in range(new_dim):
                for j_iter in range(new_dim):
                    i_st = i_iter * self.s
                    j_st = j_iter * self.s

                    for old_layer_ind in old_layers.indices:
                        for i in range(self.k):
                            for j in range(self.k):
                                push_deriv[old_layer_ind][i_st + i][j_st + j] += (
                                        self.deriv[layer_ind][i_iter][j_iter] *
                                        self.kernel[layer_ind][old_layer_ind][i][j]
                                )
                                self.kernel_deriv[layer_ind][old_layer_ind][i][j] += (
                                        self.deriv[layer_ind][i_iter][j_iter] *
                                        padded_layers[old_layer_ind][i_st + i][j_st + j]
                                )

        self.prev.add_deriv(push_deriv.map(lambda layer: self._process_deriv_layer(layer, old_dim)))

    def _process_deriv_layer(self, layer: SquareMatrix, old_dim: int) -> SquareMatrix:
        self.add_padded_deriv(layer)

        deriv_layer = []
        for i in range(self.p, self.p + old_dim):
            row = layer[i][self.p:self.p + old_dim]
            deriv_layer.append(row.copy())

        return SquareMatrix(deriv_layer)

    def add_padded_deriv(self, deriv_layer: SquareMatrix):
        raise NotImplementedError()

    def print_param_deriv(self):
        for old in self.kernel_deriv:
            for new in old.layers:
                for row in new.source:
                    for elem in row:
                        print(f"{elem} ", end=" ")
        print()


class CnvmNode(CnvxNode):
    def fill(self, dim: int, matrix: SquareMatrix):
        p = self.p
        for i in range(p):
            for j in range(p):
                matrix[i][j] = matrix[2 * p - i][2 * p - j]
            for j in range(p, dim + p):
                matrix[i][j] = matrix[2 * p - i][j]
            for j in range(dim + p, dim + 2 * p):
                matrix[i][j] = matrix[2 * p - i][2 * (dim + p - 1) - j]

        for i in range(p, dim + p):
            for j in range(p):
                matrix[i][j] = matrix[i][2 * p - j]
            for j in range(dim + p, dim + 2 * p):
                matrix[i][j] = matrix[i][2 * (dim + p - 1) - j]

        for i in range(dim + p, dim + 2 * p):
            for j in range(p):
                matrix[i][j] = matrix[2 * (dim + p - 1) - i][2 * p - j]
            for j in range(p, dim + p):
                matrix[i][j] = matrix[2 * (dim + p - 1) - i][j]
            for j in range(dim + p, dim + 2 * p):
                matrix[i][j] = matrix[2 * (dim + p - 1) - i][2 * (dim + p - 1) - j]

    def add_padded_deriv(self, deriv_layer: SquareMatrix):
        dim = deriv_layer.dim - 2 * self.p
        p = self.p

        for i in range(p):
            for j in range(p):
                deriv_layer[2 * p - i][2 * p - j] += deriv_layer[i][j]
            for j in range(p, dim + p):
                deriv_layer[2 * p - i][j] += deriv_layer[i][j]
            for j in range(dim + p, dim + 2 * p):
                deriv_layer[2 * p - i][2 * (dim + p - 1) - j] += deriv_layer[i][j]

        for i in range(p, dim + p):
            for j in range(p):
                deriv_layer[i][2 * p - j] += deriv_layer[i][j]
            for j in range(dim + p, dim + 2 * p):
                deriv_layer[i][2 * (dim + p - 1) - j] += deriv_layer[i][j]

        for i in range(dim + p, dim + 2 * p):
            for j in range(p):
                deriv_layer[2 * (dim + p - 1) - i][2 * p - j] += deriv_layer[i][j]
            for j in range(p, dim + p):
                deriv_layer[2 * (dim + p - 1) - i][j] += deriv_layer[i][j]
            for j in range(dim + p, dim + 2 * p):
                deriv_layer[2 * (dim + p - 1) - i][2 * (dim + p - 1) - j] += deriv_layer[i][j]


class CnveNode(CnvxNode):
    def fill(self, dim: int, matrix: SquareMatrix):
        p = self.p

        for i in range(p):
            for j in range(p):
                matrix[i][j] = matrix[p][p]
            for j in range(p, dim + p):
                matrix[i][j] = matrix[p][j]
            for j in range(dim + p, dim + 2 * p):
                matrix[i][j] = matrix[p][p + dim - 1]

        for i in range(p, dim + p):
            for j in range(p):
                matrix[i][j] = matrix[i][p]
            for j in range(dim + p, dim + 2 * p):
                matrix[i][j] = matrix[i][p + dim - 1]

        for i in range(dim + p, dim + 2 * p):
            for j in range(p):
                matrix[i][j] = matrix[dim + p - 1][p]
            for j in range(p, dim + p):
                matrix[i][j] = matrix[dim + p - 1][j]
            for j in range(dim + p, dim + 2 * p):
                matrix[i][j] = matrix[dim + p - 1][dim + p - 1]

    def add_padded_deriv(self, deriv_layer: SquareMatrix):
        dim = deriv_layer.dim - 2 * self.p
        p = self.p

        for i in range(p):
            for j in range(p):
                deriv_layer[p][p] += deriv_layer[i][j]
            for j in range(p, dim + p):
                deriv_layer[p][j] += deriv_layer[i][j]
            for j in range(dim + p, dim + 2 * p):
                deriv_layer[p][p + dim - 1] += deriv_layer[i][j]

        for i in range(p, dim + p):
            for j in range(p):
                deriv_layer[i][p] += deriv_layer[i][j]
            for j in range(dim + p, dim + 2 * p):
                deriv_layer[i][p + dim - 1] += deriv_layer[i][j]

        for i in range(dim + p, dim + 2 * p):
            for j in range(p):
                deriv_layer[dim + p - 1][p] += deriv_layer[i][j]
            for j in range(p, dim + p):
                deriv_layer[dim + p - 1][j] += deriv_layer[i][j]
            for j in range(dim + p, dim + 2 * p):
                deriv_layer[dim + p - 1][dim + p - 1] += deriv_layer[i][j]


class CnvcNode(CnvxNode):
    def fill(self, dim: int, matrix: SquareMatrix):
        p = self.p
        for i in range(p):
            for j in range(p):
                matrix[i][j] = matrix[dim + i][dim + j]
            for j in range(p, dim + p):
                matrix[i][j] = matrix[dim + i][j]
            for j in range(dim + p, dim + 2 * p):
                matrix[i][j] = matrix[dim + i][j - dim]

        for i in range(p, dim + p):
            for j in range(p):
                matrix[i][j] = matrix[i][dim + j]
            for j in range(dim + p, dim + 2 * p):
                matrix[i][j] = matrix[i][j - dim]

        for i in range(dim + p, dim + 2 * p):
            for j in range(p):
                matrix[i][j] = matrix[i - dim][dim + j]
            for j in range(p, dim + p):
                matrix[i][j] = matrix[i - dim][j]
            for j in range(dim + p, dim + 2 * p):
                matrix[i][j] = matrix[i - dim][j - dim]

    def add_padded_deriv(self, deriv_layer: SquareMatrix):
        dim = deriv_layer.dim - 2 * self.p
        p = self.p

        for i in range(p):
            for j in range(p):
                deriv_layer[dim + i][dim + j] += deriv_layer[i][j]
            for j in range(p, dim + p):
                deriv_layer[dim + i][j] += deriv_layer[i][j]
            for j in range(dim + p, dim + 2 * p):
                deriv_layer[dim + i][j - dim] += deriv_layer[i][j]

        for i in range(p, dim + p):
            for j in range(p):
                deriv_layer[i][dim + j] += deriv_layer[i][j]
            for j in range(dim + p, dim + 2 * p):
                deriv_layer[i][j - dim] += deriv_layer[i][j]

        for i in range(dim + p, dim + 2 * p):
            for j in range(p):
                deriv_layer[i - dim][dim + j] += deriv_layer[i][j]
            for j in range(p, dim + p):
                deriv_layer[i - dim][j] += deriv_layer[i][j]
            for j in range(dim + p, dim + 2 * p):
                deriv_layer[i - dim][j - dim] += deriv_layer[i][j]


nodes = Node.read_nodes()
output_node = nodes[-1]

layers = output_node.calc_function()
for layer in layers.layers:
    layer.print()

output_deriv = list(map(float, sys.stdin.readline().split()))
output_node.add_deriv(Matrix3D.parse_matrix_3d(layers.depth, layers.dim, output_deriv))

for node in reversed(nodes):
    node.push_deriv()

for layer in nodes[0].deriv.layers:
    layer.print()

print()
for node in nodes:
    if isinstance(node, (BiasNode, CnvxNode)):
        node.print_param_deriv()

