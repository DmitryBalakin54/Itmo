if __name__ == '__main__':
    n = int(input())
    data = [tuple(map(int, input().split())) for _ in range(n)]
    rank_x1 = {value: rank + 1 for rank, value in enumerate(sorted([x[0] for x in data]))}
    rank_x2 = {value: rank + 1 for rank, value in enumerate(sorted([x[1] for x in data]))}
    ranks1 = [rank_x1[x[0]] for x in data]
    ranks2 = [rank_x2[x[1]] for x in data]
    d_squared = [(r1 - r2) ** 2 for r1, r2 in zip(ranks1, ranks2)]
    print(1 - (6 * sum(d_squared)) / (n * (n ** 2 - 1)))
