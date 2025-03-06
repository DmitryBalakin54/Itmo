#!/usr/bin/env bash

: > logs/mem_2.log
: > logs/swap_2.log
: > logs/params1_2.log
: > logs/params2_2.log
: > logs/top5_2.log
cnt=0
while true
do
    BOTH=$(free --mega | tail -n2 | awk '{print $4;}')
    echo "$BOTH" | head -n1 >> logs/mem_2.log
    echo "$BOTH" | tail -n1 >> logs/swap_2.log

    top -b -n1 | grep -m 1 "mem.bash" >> logs/params1_2.log
    top -b -n1 | grep -m 1 "mem2.bash" >> logs/params2_2.log

    top -b -n1 -o %MEM | head -n12 | tail -n5 >> logs/top5_2.log
    echo "" >> logs/top5_2.log

    sleep 0.1s
    cnt=$((cnt + 1))
    echo "iter $cnt"
done
