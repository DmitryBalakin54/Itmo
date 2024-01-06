#!/bin/bash
fileName=$1
while read -r line;
do
	[[ $line == "-1" ]] && break
	echo "$(( 2*$line % 1000000001))" >> $fileName
done < $fileName