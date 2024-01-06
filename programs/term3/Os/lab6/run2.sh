#!/bin/bash
echo "" > "./res2.log"
for ((N = 1; N <= 20; N++)); do
    echo "$N" >> "./res2.log"
    for ((i = 1; i <= 10; i++)); do
        /usr/bin/time -o "./res2.log" -a -f "%e" "./test2.sh" $N
    done
    echo "" >> "./res2.log"
done
