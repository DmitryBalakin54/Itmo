#/usr/bin/env bash

f=script4_tmp

inf1_pid=$(cat $f | awk '$1==1 {print $2}')
inf2_pid=$(cat $f | awk '$1==2 {print $2}')
inf3_pid=$(cat $f | awk '$1==3 {print $2}')

cpulimit -p $inf1_pid -l 10 -b

kill $inf3_pid

