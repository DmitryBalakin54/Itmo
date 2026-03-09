#!/bin/bash

apt update
apt install -y nfs-kernel-server

systemctl enable nfs-server
systemctl start nfs-server
