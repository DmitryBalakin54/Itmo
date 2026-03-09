#!/bin/bash

fdisk /dev/sde
# n
# p
# 1
# Enter
# Enter
# t
# 8e
# w

vgextend vg_data /dev/sde1
lvextend -l +100%FREE -i1 /dev/vg_data/lv_vol01
