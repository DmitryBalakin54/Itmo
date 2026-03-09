#!/bin/bash

#!/bin/bash

fdisk /dev/sdc
# o
# n
# p
# 1
# Enter
# Enter
# t
# 8e
# w

fdisk /dev/sdd
# o
# n
# p
# 1
# Enter
# Enter
# t
# 8e
# w

pvcreate /dev/sdc1
pvcreate /dev/sdd1
