#!/bin/bash
cd /root/localrepo || exit 1

apt install -y dpkg-dev
dpkg-scanpackages --multiversion . /dev/null > Packages

cat <<EOF > Release
Origin: My Local Repo
Label: My Local Repo
Suite: stable
Version: 1.0
Codename: myrepo
Architectures: amd64
Components: main
Description: My local APT repository
Date: $(date -R)
EOF

