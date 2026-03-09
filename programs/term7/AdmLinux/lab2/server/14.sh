#!/bin/bash

vgcreate vg_data /dev/sdc1 /dev/sdd1
lvcreate -i 2 -l 100%FREE -n lv_vol01 vg_data
mkfs.ext4 /dev/vg_data/lv_vol01
mkdir -p /mnt/vol01
mount /dev/vg_data/lv_vol01 /mnt/vol01
