#!/bin/bash

PID=$1

if [ -z "$PID" ]; then
    echo "Usage: $0 <PID>"
    exit 1
fi

CGROUP_PATH=$(awk -F: '/0::/ {print $3}' /proc/$PID/cgroup)
CGROUP="/sys/fs/cgroup$CGROUP_PATH"

while true
do
    IDLE1=$(grep '^cpu ' /proc/stat | awk '{print $5}')
    TOTAL1=$(grep '^cpu ' /proc/stat | awk '{sum=0; for(i=2;i<=NF;i++) sum+=$i; print sum}')

    sleep 1

    IDLE2=$(grep '^cpu ' /proc/stat | awk '{print $5}')
    TOTAL2=$(grep '^cpu ' /proc/stat | awk '{sum=0; for(i=2;i<=NF;i++) sum+=$i; print sum}')

    IDLE=$((IDLE2 - IDLE1))
    TOTAL=$((TOTAL2 - TOTAL1))

    USAGE=$((100 * (TOTAL - IDLE) / TOTAL))

    if [ $USAGE -lt 20 ]; then
        echo "80000 100000" > $CGROUP/cpu.max
    elif [ $USAGE -gt 60 ]; then
        echo "30000 100000" > $CGROUP/cpu.max
    fi
done
