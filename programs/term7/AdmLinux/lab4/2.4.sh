#!/bin/bash

if systemctl is-active --quiet cron
then
    echo "done"
else
    echo "start"
    sudo systemctl start cron
fi

systemctl status cron
