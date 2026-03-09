#!/bin/bash

mkdir /mnt/vol01
mount /dev/vg_data/lv_vol01 /mnt/vol01

UUID=$(blkid -s UUID -o value /dev/vg_data/lv_vol01)
echo "UUID=$UUID /mnt/vol01 ext4 defaults 0 2" >> /etc/fstab
