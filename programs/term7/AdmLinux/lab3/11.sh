#!/bin/bash

apt-cache depends gcc | awk '/Зависит:/ {print $2}' > task11.log

