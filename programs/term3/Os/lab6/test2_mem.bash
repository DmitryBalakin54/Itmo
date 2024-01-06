#!/bin/bash

N=$1
for run in $(seq $N); do
	./mem_algo.sh $run & 
done
wait