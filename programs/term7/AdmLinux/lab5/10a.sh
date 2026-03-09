#!/bin/bash

BASE=/root/overlay77

mkdir -p $BASE/lower
mkdir -p $BASE/upper
mkdir -p $BASE/work
mkdir -p $BASE/merged

echo "Оригинальный текст из LOWER" > $BASE/lower/77_original.txt

mount -t overlay overlay \
-o lowerdir=$BASE/lower,upperdir=$BASE/upper,workdir=$BASE/work \
$BASE/merged

echo "OverlayFS смонтирован."
ls -l $BASE/merged
