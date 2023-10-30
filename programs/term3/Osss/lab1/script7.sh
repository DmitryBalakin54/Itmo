#!/usr/bin/env bash

touch emails.lst

file_out=emails.lst

grep -E -o -r -h -a '[A-Za-z0-9_.%-+]+@[A-Za-z0-9.-]+\.[A-Za-z]{2, 6}' /etc | sort | uniq > $file_out

