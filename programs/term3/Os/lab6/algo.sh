#!/bin/bash

calculate_pi() {
    local iterations=$1
    local count=0

    for ((i = 0; i < iterations; i++)); do
        x=$(bc -l <<< "scale=10; $RANDOM / 32767")
        y=$(bc -l <<< "scale=10; $RANDOM / 32767")
        distance=$(bc -l <<< "scale=10; sqrt($x * $x + $y * $y)")

        if (( $(bc <<< "$distance <= 1") == 1 )); then
            ((count++))
        fi
    done

    pi=$(bc -l <<< "scale=10; 4 * $count / $iterations")
    echo "$pi"
}

result=$(calculate_pi $((10 + $1)))
echo "Pi is approximately $result"

