#!/bin/bash

umount /mnt/newdisk

fdisk /dev/sdb
# d
# n
# p
# 1
# Enter
# +1G
# N
# w

e2fsck -f /dev/sdb1
resize2fs /dev/sdb1
mount /dev/sdb1 /mnt/newdisk
