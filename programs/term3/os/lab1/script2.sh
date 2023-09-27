
#!/usr/bin/env bash

s=""
read s
res="$s"

while [[ "$s" != "q" ]]
do

read s
res="$res $s"

done

echo "$res"
