#!/bin/bash

echo "All groups: $(awk -F: '{print $1}' /etc/group | paste -sd ',' | sed 's/,/, /g')" >> work3.log
