#!/bin/bash

ALGO_RUNNER="/path/to/runner.sh"  # Путь к скрипту запуска алгоритма
OUTPUT_FILE="execution_times_parallel.txt"

for ((N = 1; N <= 20; N++)); do
    for ((i = 1; i <= 10; i++)); do
        # Замер времени с помощью утилиты time для N вычислений в фоне
        { time $ALGO_RUNNER $N; } 2>&1 | grep real | cut -d ' ' -f 2 >> "$OUTPUT_FILE"
    done
done
