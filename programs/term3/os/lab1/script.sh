#!/usr/bin/env bash

in_file='file.csv'

for i in $*; do
    echo -n "$i: "
    awk -F ', ' -v prt=$i '$2==prt {print $3}' $in_file | awk  'BEGIN {c=0;s=0} {c++;s+=$1} END {if (c > 0) print s/c}'
done

