#/usr/bin/env bash

bash infloop.sh & inf1_pid=$!
bash infloop.sh & inf2_pid=$!
bash infloop.sh & inf3_pid=$!

touch script4_tmp
f=script4_tmp
echo "1 $inf1_pid" > $f
echo "2 $inf2_pid" >> $f
echo "3 $inf3_pid" >> $f



