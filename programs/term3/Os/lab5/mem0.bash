#!/bin/bash

cnt=0
array=()

while true
do
    array+=(0 1 2 3 4 5 6 7 8 9)
    cnt=$(( cnt + 1 ))
    if ! (( cnt % 100000 ))
    then
        echo ${#array[@]}
    fi
done
