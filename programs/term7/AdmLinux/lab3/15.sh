#!/bin/bash


mkdir -p /usr/local/localrepo
cp -r /root/localrepo/* /usr/local/localrepo/


echo "deb [trusted=yes] file:/usr/local/localrepo ./" > /etc/apt/sources.list.d/localrepo.list
apt update
