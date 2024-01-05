#/usr/bin/env bash

f=script4_tmp

cat $f | awk '{print $2}' | xargs kill -9
rm -f $f
