#!/usr/bin/env bash

shopt -s extglob

mkfifo pipe
while true; do
    read l
    echo "$l" > pipe

    case "$l" in

        QUIT)
        rm pipe
        exit 0
        ;;

        !(+([0-9])|\*|+))
        echo "Invalid input"
        rm pipe
        exit -1
        ;;

    esac
done

rm pipe
