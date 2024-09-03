import os

os.system('pip install networkx')
os.system('pip install sympy')


import networkx
import sympy


n, m = map(int, input().split(' '))
edges = []
for i in range(m):
    u, v = map(int, input().split(' '))
    edges.append((u, v))
    edges.append((v, u))

G = networkx.Graph()
G.add_edges_from(edges)
chrom_poly = networkx.chromatic_polynomial(G)
cf = sympy.Poly(chrom_poly).all_coeffs()
print(len(cf) - 1)
print(*cf)
