#!/bin/bash

BASE=/root/overlay77
LOG=$BASE/77_audit.log

echo "Whiteout файлы:" > $LOG
find $BASE/upper -name ".wh.*" >> $LOG

echo "" >> $LOG
echo "Различия между lower и merged:" >> $LOG
diff -r $BASE/lower $BASE/merged >> $LOG

echo "Отчет создан: $LOG"
