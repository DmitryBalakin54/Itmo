#!/usr/bin/env bash

out=task4_out

touch tmp

cat $out > tmp
echo -n > $out

read s < tmp
ppid=$(echo $s | sed -r 's/(:|=)/ /g'| awk  '{print $4}')
art=0
art_count=0
cur_ppid=-1
cur_art=-1
while read s; do
    vals=$(echo $s | sed -r 's/(:|=)/ /g'| awk  '{print $4, $6}')
    cur_ppid=$(echo $vals | awk '{print $1}')
    cur_art=$(echo $vals | awk '{print $2}')
    if [[ "$cur_ppid" == "$ppid" ]]; then
        art_count=$((art_count + 1))
        art=$(echo "$cur_art" "$art" | awk '{print $1+$2}')
    else
        echo -n "Average_Running_Children_of_ParentID=$ppid is " >> $out
        echo "$art" "$art_count" | awk '{print $1/$2}' >> $out
        art_count=1
        art=$cur_art
        ppid=$cur_ppid
    fi
    echo $s >> $out
done < tmp

echo -n "Average_Running_Children_of_ParentID=$ppid is " >> $out
echo "$art" "$art_count" | awk '{print $1/$2}' >> $out

rm -f tmp
