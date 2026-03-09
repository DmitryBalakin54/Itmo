#!/bin/bash

CGROUP="/sys/fs/cgroup/backup-77"

echo "+io" > /sys/fs/cgroup/cgroup.subtree_control

mkdir -p $CGROUP

DEVICE=$(lsblk -dno MAJ:MIN | head -n1)

echo "$DEVICE riops=1770 wiops=1270" > $CGROUP/io.max

echo $$ > $CGROUP/cgroup.procs

dd if=/dev/zero of=testfile77 bs=4K count=500000 oflag=direct
