#!/bin/bash

N=3900000
K=30

for ((i=0; i<K; i++))
do
    ./newmem.bash $N $i &
done
