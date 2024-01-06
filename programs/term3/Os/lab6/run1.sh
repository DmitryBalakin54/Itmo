#!/bin/bash
echo "" > "./res1.log"
for ((N = 1; N <= 20; N++)); do
    echo "$N" >> "./res1.log"
    for ((i = 1; i <= 10; i++)); do
        /usr/bin/time -o "./res1.log" -a -f "%e" "./algo.sh" $N
    done
    echo "" >> "./res1.log"
done
