#!/bin/bash

apt-cache madison htop | awk '{print $3}' | sort -u > task16.log
