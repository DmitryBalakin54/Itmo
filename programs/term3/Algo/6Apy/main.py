import heapq as hq

def calculate_time(n, c, s, e):
    SCALE = 10**4

    d_s = lambda c1, c2: (c1[0] - c2[0]) ** 2 + (c1[1] - c2[1]) ** 2

    def scale_coordinates(coord):
        return (coord[0] + SCALE, coord[1] + SCALE)

    c = [scale_coordinates(coord) for coord in c]

    g = {i + 1: [(j + 1, d_s(c[i], c[j])) for j in range(n) if j != i] for i in range(n)}

    d = [float('inf')] * (n + 1)
    d[s] = 0

    heap = [(0, s)]
    processed = set()

    while heap:
        cd, cc = hq.heappop(heap)

        if cc in processed:
            continue

        processed.add(cc)

        for ne, w in g[cc]:
            nd = cd + w

            if nd < d[ne]:
                hq.heappush(heap, (nd, ne))
                d[ne] = nd

    return d[e]

if __name__ == "__main__":
    n = int(input())
    c = [tuple(map(int, input().split())) for _ in range(n)]
    s, e = map(int, input().split())

    result = calculate_time(n, c, s, e)
    print(result)
