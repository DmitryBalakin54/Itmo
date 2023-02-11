import string
n=int(input())
a=input().split()
b=list(string.ascii_lowercase)
i=0
buf=""

while i < n:
    buf+=b[int(a[i])]
    try:
        b.index(buf)
        print(b[int(a[i])], end = "")
    except ValueError:
        #b.append(buf)
        b.append(buf[len(buf) - 1] + b[int(a[i - 1])[0]])
        buf=""
        i-=1
    i+=1
