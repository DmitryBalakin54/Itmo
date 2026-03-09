#!/bin/bash

systemctl enable mnt-mydata.automount
systemctl start mnt-mydata.automount

systemctl status mnt-mydata.automount

ls /mnt/mydata
mount | grep mydata
