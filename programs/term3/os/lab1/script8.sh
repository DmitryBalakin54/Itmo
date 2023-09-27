#!/usr/bin/env bash

file_in=/etc/passwd

awk -F ':' '{print $1, $3}' $file_in | sort -k 2n

