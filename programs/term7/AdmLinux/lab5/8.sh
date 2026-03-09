#!/bin/bash

unshare --mount --fork bash -c "
mount --make-rprivate /
DIR=/tmp/private_77
mkdir -p \$DIR
mount -t tmpfs tmpfs \$DIR
echo 'Внутри namespace:'
df -h | grep private_77
sleep 30
"
