class TrieNode:
    def __init__(self):
        self.clds = {}
        self.cnt = 0
        self.max_xor = 0


def add(root, num):
    node = root
    for i in range(31, -1, -1):
        bit = (num >> i) & 1
        if bit not in node.clds:
            node.clds[bit] = TrieNode()
        node = node.clds[bit]
        node.cnt += 1


def pop(root, num):
    node = root
    for i in range(31, -1, -1):
        bit = (num >> i) & 1
        node = node.clds[bit]
        node.cnt -= 1


def max_xor(root, num):
    nd = root
    res = 0
    for i in range(31, -1, -1):
        bit = (num >> i) & 1
        neg_bit = 1 - bit
        if neg_bit in nd.clds and nd.clds[neg_bit].cnt > 0:
            res |= (1 << i)
            nd = nd.clds[neg_bit]
        else:
            nd = nd.clds[bit]
    return res


def ans(qq):
    root = TrieNode()
    add(root, 0)
    res = []

    for op, val_s in qq:
        num = int(val_s)
        if op == '+':
            add(root, num)
        elif op == '-':
            pop(root, num)
        elif op == '?':
            res.append(max_xor(root, num))

    return res


q = int(input())
qq = [input().split() for _ in range(q)]

for r in ans(qq):
    print(r)
