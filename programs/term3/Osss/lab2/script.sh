#/usr/env/bin bash

ps aux | awk -v u=$USER '{if ($1==u && $8=="Z") print $2}'
