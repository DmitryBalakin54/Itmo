#!/bin/bash

systemctl disable mnt-mydata.mount
systemctl stop mnt-mydata.mount
mount | grep mydata
