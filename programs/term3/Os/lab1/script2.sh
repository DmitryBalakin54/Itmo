
#!/usr/bin/env bash

s=""
read s

while [[ "$s" != "q" ]]; do

    res+=$s
    read s

done

echo "$res"
