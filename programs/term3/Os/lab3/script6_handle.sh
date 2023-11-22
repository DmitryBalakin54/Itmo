#!/usr/bin/env bash

val=1
mode="undef"

usr1()
{
    mode="+"
}

usr2()
{
    mode="*"
}

term()
{
    mode="ex"
}

echo $$ > pid

trap 'usr1' USR1
trap 'usr2' USR2
trap 'term' SIGTERM

while true; do
    sleep 1

    case "$mode" in

        +)
        val=$(($val + 2))
        echo $val
        ;;

        \*)
        val=$(($val * 2))
        echo $val
        ;;

        ex)
        echo "Termination of work on the signal of the generator"
        exit 0
        ;;

    esac
done
