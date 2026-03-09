#!/bin/bash

last=$(chage -l root | grep "Last password" | cut -d: -f2)

echo "Password for root changed last time on$last" >> work3.log
