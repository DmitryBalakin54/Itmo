#!/bin/bash

ID=77
USER_NAME="user-77"
CGROUP="/sys/fs/cgroup/user-77"

useradd -m $USER_NAME 2>/dev/null

echo "+cpu" > /sys/fs/cgroup/cgroup.subtree_control

mkdir $CGROUP

echo "70000 100000" > $CGROUP/cpu.max
