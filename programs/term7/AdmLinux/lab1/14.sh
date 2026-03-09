#!/bin/bash


dir="/home/test13"

groupadd test13_g

usermod -aG test13_g u1
usermod -aG test13_g u2

chown u1:test13_g "$dir"
chmod u=rwx,g=rx,o= "$dir"

chown u1:test13_g "$dir"/work3-1.log
chmod u=rw,g=r,o= "$dir"/work3-1.log

chown u1:test13_g "$dir"/work3-2.log
chmod u=rw,g=r,o= "$dir"/work3-2.log


