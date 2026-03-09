#!/bin/bash

CGROUP="/sys/fs/cgroup/cpuset-77"

echo "+cpuset" > /sys/fs/cgroup/cgroup.subtree_control

mkdir -p $CGROUP

echo 0 > $CGROUP/cpuset.cpus
echo 0 > $CGROUP/cpuset.mems

echo $$ > $CGROUP/cgroup.procs

top
