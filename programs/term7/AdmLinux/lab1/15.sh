#!/bin/bash

dir="/home/test14"

mkdir -p "$dir"

chown u1 "$dir"

chmod a=rwx,o+t "$dir"
