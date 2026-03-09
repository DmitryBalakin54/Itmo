#!/bin/bash

systemctl is-enabled mnt-mydata.mount
systemctl is-enabled mnt-mydata.automount
systemctl status mnt-mydata.automount
mount | grep mydata
ls /mnt/mydata
mount | grep mydata
sleep 20
mount | grep mydata
