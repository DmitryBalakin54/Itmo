#!/usr/bin/env bash

val=$1

for i in $2 $3
do

if [[ $val -le $i ]]
then
val=$i
fi

done

echo $val

