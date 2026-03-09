#!/bin/bash

ls -la /var/remotenfs
echo "Test file from client" > ~/testfile.txt
cp ~/testfile.txt /var/remotenfs/
cat /var/remotenfs/testfile.txt
ls -la /var/remotenfs
