#!/bin/bash

UUID=$(blkid -s UUID -o value /dev/sdb1)

echo "UUID=$UUID /mnt/newdisk ext4 defaults,noexec,noatime 0 2" >> /etc/fstab
