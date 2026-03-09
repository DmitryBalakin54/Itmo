#!/bin/bash

cat > /etc/systemd/system/mymsg.service <<EOF
[Unit]
Description=Message service
After=network-online.target
Wants=network-online.target

[Service]
Type=oneshot
ExecStart=/bin/bash -c 'echo MyMsg started at $(date) | logger'
RemainAfterExit=yes


ExecStartPost=/usr/bin/touch /run/mymsg.started
ExecStartPost=/usr/bin/touch /run/mymsg.running
ExecStartPost=/usr/bin/rm -f /run/mymsg.stopped

ExecStop=/usr/bin/rm -f /run/mymsg.running
ExecStopPost=/usr/bin/touch /run/mymsg.stopped

[Install]
WantedBy=multi-user.target
EOF

systemctl daemon-reload
systemctl enable mymsg
