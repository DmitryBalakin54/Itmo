#!/usr/bin/env bash

shopt -s extglob

val=1
mode="+"

(tail -f pipe) | while true; do
    read l

    case "$l" in

        QUIT)
        echo "Terminate"
        killall tail
        exit 0
        ;;

        \*)
        mode="*"
        ;;

        +)
        mode="+"
        ;;

        +([0-9]))
        val=$(($val $mode $l))
        echo $val
        ;;

        *)
        echo "Invalid input"
        killall tail
        exit -1
        ;;

    esac
done
