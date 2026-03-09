#!/bin/bash

blkid /dev/sdb1 | awk '{print $2}' | tr -d '"' > ~/partition_uuid.txt
