#!/bin/bash

bash -c 'cat > /etc/systemd/system/mnt-mydata.mount <<EOF
[Unit]
Description=Mount MyData Partition
After=local-fs.target
Requires=local-fs.target

[Mount]
What=/root/mydisk.img
Where=/mnt/mydata
Type=ext4
Options=loop

[Install]
WantedBy=multi-user.target
EOF'

systemctl daemon-reload
