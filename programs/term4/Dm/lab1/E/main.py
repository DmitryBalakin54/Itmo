from collections import defaultdict

binomial_coefficients = defaultdict(dict)


def calculate_polynomial_term(power):
    powers = [i ** power for i in range(power + 1)]
    polynomial_term = []
    for k in range(1, power + 2):
        actual = 0
        for i in range(power + 1):
            sign = 1 if (i + 2) % 2 == 0 else -1
            actual += get_value(powers, k - i - 2) * sign * calculate_binomial_coefficient(power + 1, i + 1)
        polynomial_term.append(powers[k - 1] - actual)
    return polynomial_term


add_polynomials = (lambda polynomial1, polynomial2: [get_value(polynomial1, i) + get_value(polynomial2, i) for i in
                                                     range(max(len(polynomial1), len(polynomial2)))])

multiply_polynomials = (lambda polynomial1, polynomial2: [
    sum(get_value(polynomial1, j) * get_value(polynomial2, i - j) for j in range(i + 1)) for i in
    range(len(polynomial1) + len(polynomial2))])


def print_polynomial(polynomial):
    degree = next(i for i in range(len(polynomial) - 1, -1, -1) if polynomial[i] != 0)
    print(degree)
    for i in range(degree + 1):
        print(get_value(polynomial, i), end=" ")
    print()


get_value = (lambda sequence, index: sequence[index] if 0 <= index < len(sequence) else 0)


def calculate_binomial_coefficient(n, k):
    if k > n:
        return 0
    if k in binomial_coefficients[n]:
        return binomial_coefficients[n][k]
    result = 1
    for i in range(k):
        result = (result * (n - i)) // (i + 1)
    binomial_coefficients[n][k] = result
    return result


def make_polynomial(polynomial, value):
    power = 1
    for i in range(len(polynomial)):
        polynomial[i], power = power * polynomial[i], value * power


if __name__ == "__main__":
    value, degree = int(input()), int(input()) + 1
    coefficients = list(map(int, input().split()))

    one_minus_s, one_minus_s_power = [1, -1], [1, -1]
    polynomial = [coefficients[0]]

    for i in range(0, degree - 1):
        polynomial = add_polynomials(multiply_polynomials(polynomial, one_minus_s),
                                     multiply_polynomials(calculate_polynomial_term(i + 1), [coefficients[i + 1]]))
        one_minus_s_power = multiply_polynomials(one_minus_s, one_minus_s_power)

    make_polynomial(polynomial, value)
    make_polynomial(one_minus_s_power, value)

    print_polynomial(polynomial)
    print_polynomial(one_minus_s_power)
