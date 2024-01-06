#!/bin/bash

calculate_sum_of_squares() {
    local n=$(($1 * $1 * $1 * $1))
    local sum=0

    for ((i = 1; i <= n; i++)); do
        square=$((i * i))
        sum=$((sum + square))
    done

    echo "$sum"
}

N=$1
result=$(calculate_sum_of_squares "$N")
echo "Результат вычисления суммы квадратов от 1 до $(($N * $N * $N * $N)): $result"
