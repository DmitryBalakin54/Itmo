#!/usr/bin/env bash

touch task1_out

out=task1_out

res=$(ps xu | tail -n +2 | awk '{print $2 ":" $11}')

wc -l <<< $res > $out
echo "$res" >> $out

