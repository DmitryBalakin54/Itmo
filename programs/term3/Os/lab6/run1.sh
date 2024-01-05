#!/bin/bash

for ((N = 1; N <= 20; N++)); do
    for ((i = 1; i <= 10; i++)); do
        /usr/bin/time -o "./res1.log" -a -f "%e" "test1.sh" $N
    done
done
