#!/usr/bin/env bash

touch info.log

file_out=info.log
file_in=/var/log/anaconda/syslog

awk -F ' ' '$2=="INFO"' $file_in > $file_out
