#!/usr/bin/env bash

touch task2_out

out=task2_out

ps aux | awk '{if ($11 ~ /^\/sbin\/+/) print $2}' > $out

