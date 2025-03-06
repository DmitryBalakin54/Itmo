#!/bin/bash

cnt=0
array=()

while (( ${#arr[@]} < $1 ))
do
    array+=(0 1 2 3 4 5 6 7 8 9)
    cnt=$(( cnt + 1 ))
done

echo "end $2" >> report_newmem.log
