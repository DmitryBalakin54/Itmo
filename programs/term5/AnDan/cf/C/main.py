def pearson_correlation(N, K, a, b):
    category_sum_b = [0] * K
    category_count = [0] * K
    total_b = sum(b)
    total_mean_b = total_b / N

    for i in range(N):
        category = a[i] - 1
        category_sum_b[category] += b[i]
        category_count[category] += 1

    result = 0

    for k in range(K):
        if category_count[k] > 0:
            weight = category_count[k] / N

            numerator = 0
            sum_xk_squared = 0
            sum_b_dev_squared = 0

            for i in range(N):
                if a[i] - 1 == k:
                    x_k = 1
                else:
                    x_k = 0

                sum_xk_squared += (x_k - weight) ** 2
                sum_b_dev_squared += (b[i] - total_mean_b) ** 2
                numerator += (x_k - weight) * (b[i] - total_mean_b)

            denominator = (sum_xk_squared * sum_b_dev_squared) ** 0.5
            if denominator > 0:
                result += weight * (numerator / denominator)

    return result


if __name__ == "__main__":
    N, K = map(int, input().split())
    a = list(map(int, input().split()))
    b = list(map(int, input().split()))

    print(pearson_correlation(N, K, a, b))