#!/usr/bin/env bash

while true; do
    echo "1: open nano"
    echo "2: open vi"
    echo "3: open links"
    echo "4: exit"
    echo "enter the command number"

    cmd=""
    read cmd

    case $cmd in

        "1")
	nano
	;;

	"2")
	vi
	;;

	"3")
	links
	;;

	"4")
	exit
	;;

	*)
	echo "try again"
	;;

    esac

done
