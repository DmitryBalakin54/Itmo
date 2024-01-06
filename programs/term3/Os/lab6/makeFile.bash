#!/bin/bash
file=$1
ind=2
end=$2
echo 1 > $file
while true
do
	echo $ind >> $file
	if [ $ind -gt $end ] 
	then
		break
	fi
	ind=$(($ind + 1))
done

echo "-1" >> $file