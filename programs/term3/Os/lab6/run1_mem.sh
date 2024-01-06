#!/bin/bash

N=20
report="res1_mem.log"
: > $report
for run in $(seq $N); do
	echo "$run" >> $report 
	for run2 in $(seq 10); do
		./makeFiles.bash $run
		/usr/bin/time -o "$report" -a -f "%e" "./test1_mem.bash" $N
	done
done