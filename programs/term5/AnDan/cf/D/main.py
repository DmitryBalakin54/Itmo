from collections import defaultdict

if __name__ == '__main__':
    K = int(input())
    N = int(input())

    sum_y = defaultdict(int)
    sum_y_squared = defaultdict(int)
    count_x = defaultdict(int)

    index = 2
    for _ in range(N):
        x, y = map(int, input().split(' '))
        index += 2
        sum_y[x] += y
        sum_y_squared[x] += y * y
        count_x[x] += 1

    conditional_variance = 0
    for x in range(1, K + 1):
        if count_x[x] > 0:
            mean_y = sum_y[x] / count_x[x]
            variance_y = (sum_y_squared[x] / count_x[x]) - (mean_y ** 2)
            conditional_variance += count_x[x] / N * variance_y

    print(f"{conditional_variance}")
