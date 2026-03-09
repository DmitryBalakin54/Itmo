#!/bin/bash

apt-cache rdepends libgpm2 | tail -n +3 | awk '{$1=$1; print}' > task12.log

