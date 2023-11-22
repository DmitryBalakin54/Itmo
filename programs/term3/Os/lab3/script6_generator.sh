#!/usr/bin/env bash

while true; do
    read l

    case "$l" in

        +)
        kill -USR1 $(cat pid)
        ;;

        \*)
        kill -USR2 $(cat pid)
        ;;

        TERM)
        kill -SIGTERM $(cat pid)
        exit 0
        ;;

    esac
done
