import math
from fractions import Fraction

n = int(input())
amount = {}
begin = {}
end = {}
string = input()
length = len(string)
OFFSET = ord("a")

for i in range(n):
    amount[i] = 0

for i in string:
    amount[ord(i) - OFFSET] += 1

begin[0] = Fraction(0, length)
end[0] = Fraction(amount[0], length)
for i in range(n - 1):
    begin[i + 1] = end[i]
    end[i + 1] = begin[i + 1] + Fraction(amount[i + 1], length)

resultBegin = Fraction(0, 1)
resultEnd = Fraction(1, 1)
for i in string:
    rangeBE = resultEnd - resultBegin
    resultEnd = resultBegin + end[ord(i) - OFFSET] * rangeBE
    resultBegin = resultBegin + begin[ord(i) - OFFSET] * rangeBE

print(n)
for i in amount:
    print(amount.get(i) , " ", end = "")
print()
for two in range(1, 1000000000000000000000000000000000000000000):
    dot = Fraction(math.floor(resultBegin * 2 ** two), 2 ** two)
    while dot < resultBegin:
        dot += Fraction(1, 2 ** two)
    bestDot = Fraction(dot.numerator, dot.denominator)
    # while dot < resultEnd:
    #     dot += Fraction()
    if dot < resultEnd:
        s = bin(int(dot.numerator))[2:]
        print("0" * (two - len(s)) + s)
        break

# print(begin)
# print(end)