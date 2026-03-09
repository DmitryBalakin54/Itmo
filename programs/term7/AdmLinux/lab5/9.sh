#!/bin/bash

unshare --net --fork bash -c "
echo 'Сетевые интерфейсы внутри namespace:'
ip addr
echo
echo 'Пробуем ping 8.8.8.8:'
ping -c 2 8.8.8.8
sleep 20
"
