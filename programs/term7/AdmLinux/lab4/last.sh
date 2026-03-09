#!/bin/bash

cat > /etc/systemd/system/mysrv.service <<EOF
[Unit]
Description=My Second Service
After=mymsg.service

ConditionPathExists=/run/mymsg.started

ConditionPathExists=/run/mymsg.stopped

ConditionPathExists=!/run/mymsg.running

[Service]
Type=oneshot
ExecStart=/bin/bash -c 'echo mysrv executed at $(date) | logger'
EOF

systemctl daemon-reload

