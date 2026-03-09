#!/bin/bash

dpkg -l | awk '/^ii/ {print $2, $3}' > task10.log

