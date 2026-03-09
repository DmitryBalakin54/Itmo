#!/bin/bash

cd ~/src_bastet/bastet-* || exit 1

apt build-dep -y bastet

make

