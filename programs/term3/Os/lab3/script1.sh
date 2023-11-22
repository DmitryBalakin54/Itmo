#/usr/bin/env bash
time=$(date "+%d-%m-%Y-%H:%M:%S")
url="www.net_nikogo.ru"

mkdir ~/test 2>/dev/null &&
    touch ~/report &&
    echo "catalog test was created succesfully" >> ~/report &&
    touch ~/test/"$time"

ping $url 2>/dev/null || echo "$(date "+%d-%m-%Y-%H:%M:%S") couldn't connect to $url" >> ~/report

