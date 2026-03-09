#!/bin/bash

dir="/home/test15"

mkdir "$dir"

echo "secret data" > "$dir/secret_file"

chown root:root "$dir"
chmod u=rwx,g=,o=x "$dir"

chmod a+r "$dir/secret_file"
