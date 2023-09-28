#!/usr/bin/env bash

touch tmp1

for i in /proc/[[:digit:]]*; do
    echo -n $(basename $i) " "
    awk '{if ("read_bytes:"==$1) print $2}' $i/io
done > tmp1

sleep 60

touch tmp2

while read i; do
    pid=$(echo $i | awk '{print $1}')
    val=$(echo $i | awk '{print $2}')
    new_val=$(awk '{if ("read_bytes:"==$1) print $2}' /proc/$pid/io)
    res=$((new_val - val))
    echo "$pid" "$res" >> tmp2
done < tmp1

sort -nk 2 tmp2 | tail -3 | awk '{print $1}'

rm -f tmp1
rm -f tmp2
