#!/bin/bash

N=20
report="res2_mem.log"
: > $report
for run in $(seq $N); do
        echo "" >> $report
	echo "$run" >> $report
	for run2 in $(seq 10); do
		/usr/bin/time -o "$report" -a -f "%e" "./test2_mem.bash" $run
	done
done
