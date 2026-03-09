#!/bin/bash

while IFS=: read username _ id _; do
	echo "user $username has id $id" >> work3.log
done < /etc/passwd 
