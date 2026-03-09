#!/bin/bash


useradd nfsnobody -s /usr/sbin/nologin -M -U
usermod -G nfsnobody nfsnobody
chown -R nfsnobody:nfsnobody /mnt/vol01
echo "/mnt/vol01 10.0.2.0/24(rw,sync,no_subtree_check,no_root_squash)" >> /etc/exports
exportfs -ra
systemctl restart nfs-server
