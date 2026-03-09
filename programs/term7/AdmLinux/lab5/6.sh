#!/bin/bash

unshare --uts --fork bash -c "
hostname isolated-student-77
echo 'Внутри namespace hostname:'
hostname
sleep 30
"
