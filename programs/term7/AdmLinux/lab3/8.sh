#!/bin/bash

cd ~/src_bastet/bastet-* || exit 1

cp bastet /usr/local/bin/

chmod u=rwx,g=rx,o=rx /usr/local/bin/bastet

ls -l /usr/local/bin/bastet
