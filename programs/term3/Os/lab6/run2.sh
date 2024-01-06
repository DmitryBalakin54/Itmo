#!/bin/bash

ALGO_RUNNER="./test2.sh"
OUTPUT_FILE="./res2.log"
: > "./res2.log"
for ((N = 1; N <= 20; N++)); do
    echo "" >> "$OUTPUT_FILE"
    echo "$N" >> "$OUTPUT_FILE"
    for ((i = 1; i <= 10; i++)); do
        { time $ALGO_RUNNER $N; } 2>&1 | grep "real" | awk '{print $2}' | cut -c 3- | rev | cut -c 2- | rev  >> "$OUTPUT_FILE"
    done
done
