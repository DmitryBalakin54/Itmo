#!/bin/bash

echo -n "user u1: u1(UID:$(id -u u1)), groups: " >> work3.log

id -G u1 | tr ' ' '\n' | while read -r gid; do
    echo -n "$(getent group "$gid" | cut -d: -f1)(GID:$gid), " >> work3.log
done

truncate -s-2 work3.log 2>/dev/null || sed -i '$ s/, $//' work3.log

echo "" >> work3.log
