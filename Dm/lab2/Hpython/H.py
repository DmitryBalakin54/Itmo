from fractions import Fraction

OFFSET = ord('a')

n = int(input())
amoount = {}

s = input()
index = 0
length = 0
for i in s.split(" "):
    amoount[index] = int(i)
    length += amoount[index]
    index += 1

begin = {}
end = {}

begin[0] = Fraction(0, 1)
end[0] = Fraction(amoount[0], length)
for i in range(1, n):
    end[i] = end[i - 1] + Fraction(amoount[i] , length)
    begin[i] = end[i - 1]

value = input()
denominator = 2 ** len(value)
value = int(value, 2)
dot = Fraction(value, denominator)

beginCopy = begin.copy()
endCopy = end.copy()
for i in range(length):
    ran = 1
    beg = 0
    for j in range(len(begin)):
        if begin[j] <= dot < end[j]:
            print(chr(j + OFFSET), end = "")
            ran = end[j] - begin[j]
            beg = begin[j]
            break
    for j in range(len(begin)):
        end[j] = beg + endCopy[j] * ran
        begin[j] = beg + beginCopy[j] * ran
