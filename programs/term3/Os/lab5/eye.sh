#!/usr/bin/env bash

: > logs/mem.log
: > logs/swap.log
: > logs/params1.log
: > logs/top5.log
cnt=0
while true
do
    BOTH=$(free --mega | tail -n2 | awk '{print $4;}')
    echo "$BOTH" | head -n1 >> logs/mem.log
    echo "$BOTH" | tail -n1 >> logs/swap.log

    top -b -n1 | grep -m 1 "mem.bash" >> logs/params1.log

    top -b -n1 -o %MEM | head -n12 | tail -n5 >> logs/top5.log
    echo "" >> logs/top5.log

    sleep 0.1s
    cnt=$((cnt + 1))
    echo "iter $cnt"
done
