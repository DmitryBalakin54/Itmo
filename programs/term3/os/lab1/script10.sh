#!/usr/bin/env bash

man bash | tr -cs '[[:alpha:]]' '\n' | tr '[:upper:]' '[:lower:]' | grep -E '^[[:alpha:]]{4,}$' | sort | uniq -c | sort -n -r | head -3 | awk -F ' ' '{print $1, $2}'
