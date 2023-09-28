#!/usr/bin/env bash

touch tmp

for i in /proc/[[:digit:]]*; do
    echo -n $(basename $i) " "
    awk '{print $6}' $i/statm
done > tmp

sort -nk 2 tmp | tail -1 | awk '{print $1}'
top -o %MEM -b -n 1 | head -10 | tail -1 | awk '{print $1}'
rm -f tmp
