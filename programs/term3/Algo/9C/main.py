class Node:
    def __init__(self):
        self.cld = {}
        self.is_end = False
        self.cnt = 0

class Dct:
    def __init__(self):
        self.root = Node()
        self.wrds = []

    def add(self, word):
        node = self.root
        for char in word:
            if char not in node.cld:
                node.cld[char] = Node()
            node = node.cld[char]
            node.cnt += 1
        node.is_end_of_word = True

        l, r = 0, len(self.wrds)
        while l < r:
            m = (l + r) // 2
            if self.wrds[m] < word:
                l = m + 1
            else:
                r = m
        self.wrds.insert(l, word)

    def find(self, k):
        return self.wrds[k - 1]


n = int(input())
dct = Dct()

for _ in range(n):
    op, wrd = input().split()
    if op == "1":
        dct.add(wrd)
    elif op == "2":
        k = int(wrd)
        result = dct.find(k)
        print(result)
