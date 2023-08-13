import math
from fractions import Fraction
n = int(input())
line = input()
countOfSymbols = {}
startOfSymbols = {}
endOfSymbols = {}
length = len(line)
alphabet = 'abcdefghijklmnopqrstuvwxyz'
for i in range(n):
    countOfSymbols[alphabet[i]] = 0
for ch in line:
    countOfSymbols[ch] += 1
i = 0
print(n)
for key in sorted(countOfSymbols):
    startOfSymbols[key] = Fraction(i, length)
    print(countOfSymbols.get(key), end=" ")
    i += countOfSymbols.get(key)
    endOfSymbols[key] = Fraction(i, length)
sorted(startOfSymbols)
sorted(endOfSymbols)
start = Fraction(startOfSymbols.get(line[0]))
end = Fraction(endOfSymbols.get(line[0]))
line = line[1:]
print()
for ch in line:
    newStart = start + (end - start) * startOfSymbols.get(ch)
    newEnd = start + (end - start) * endOfSymbols.get(ch)
    end = newEnd
    start = newStart
for pow in range(1, 1000000000000000000000000):
    position = math.floor(start * 2**pow)
    while position < start * 2**pow:
        position +=1
    if position < end * 2**pow:
        s = bin(int(position))[2:]
        print("0" * (pow - len(s)) + s)
        break

print(startOfSymbols)
print(endOfSymbols)