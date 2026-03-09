#!/bin/bash

umount /mnt/newdisk

fdisk /dev/sdb
# n
# p
# 2
# Enter
# +12M
# t
# 2
# 83
# w

tune2fs -O ^has_journal /dev/sdb1
tune2fs -j -J device=/dev/sdb2 /dev/sdb1
mount /dev/sdb1 /mnt/newdisk
