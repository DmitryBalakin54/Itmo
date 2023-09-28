#!/usr/bin/env bash

touch task4_out

out=task4_out

touch tmp

for i in /proc/[[:digit:]]*; do
    pid=$(basename "$i")
    ppid=$(awk  '{if ($1=="PPid:") print $2}' "$i"/status)
    sert=$(awk '{if ($1=="se.sum_exec_runtime") print $3}' "$i"/sched)
    sw=$(awk '{if ($1=="nr_switches") print $3}' "$i"/sched)
    art=$(echo "$sert $sw" | awk '{print $1/$2}')
    echo "$pid $ppid $art" >> tmp
done

echo -n > $out
sort -nk 2 tmp | awk '{print "ProcessID="$1 " : Parent_ProcessID="$2 " : Average_Running_Time="$3}' >> $out

rm -f tmp
