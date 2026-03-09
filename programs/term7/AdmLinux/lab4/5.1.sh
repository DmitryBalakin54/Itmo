#!/bin/bash

dd if=/dev/zero of=/root/mydisk.img bs=1M count=200

mkfs.ext4 /root/mydisk.img

mkdir -p /mnt/mydata
