#!/usr/bin/env bash

if [[ "$HOME" == "$PWD" ]]; then

    echo "$HOME"
    exit 0

fi

echo "ERROR: current dir must be $HOME, have $PWD"
exit 1
