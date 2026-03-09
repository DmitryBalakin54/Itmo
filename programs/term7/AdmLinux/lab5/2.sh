#!/bin/bash

ID=77
CGROUP="/sys/fs/cgroup/memory-77"

LIMIT_MB=1270
LIMIT_BYTES=$((LIMIT_MB*1024*1024))

echo "+memory" > /sys/fs/cgroup/cgroup.subtree_control

mkdir $CGROUP
echo $LIMIT_BYTES > $CGROUP/memory.max

stress --vm 1 --vm-bytes 1500M --vm-keep &
PID=$!

echo $PID > $CGROUP/cgroup.procs

sleep 10

cat $CGROUP/memory.current
