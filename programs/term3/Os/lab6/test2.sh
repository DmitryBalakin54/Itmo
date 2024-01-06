#!/bin/bash

for ((i = 1; i <= $1; i++)); do
    "./algo.sh" "$i" &
done
