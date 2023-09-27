#!/usr/bin/env bash

touch full.log

file_out=full.log
file_in=/var/log/anaconda/X.log

awk -F ' ' '$3=="(WW)"' $file_in | sed 's/(WW)/Warning/g' > $file_out
awk -F ' ' '$3=="(II)"' $file_in | sed 's/(II)/Information/g' > $file_out

