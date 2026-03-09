#!/bin/bash

echo "g1 members: $(getent group g1 | cut -d: -f4 | sed 's/,/, /g')" >> work3.log
