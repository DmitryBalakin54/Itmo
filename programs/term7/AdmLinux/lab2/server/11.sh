#!/bin/bash

umount /mnt/newdisk
e2fsck -n /dev/sdb1
mount /dev/sdb1 /mnt/newdisk
