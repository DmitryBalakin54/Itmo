#!/bin/bash

unshare --pid --fork --mount-proc bash -c "
echo 'Процессы внутри PID namespace:'
ps aux
sleep 30
"
