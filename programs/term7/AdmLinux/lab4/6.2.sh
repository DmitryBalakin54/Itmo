#!/bin/bash

bash -c 'cat > /etc/systemd/system/mnt-mydata.automount <<EOF
[Unit]
Description=Automount MyData

[Automount]
Where=/mnt/mydata
TimeoutIdleSec=15

[Install]
WantedBy=multi-user.target
EOF'

systemctl daemon-reload
