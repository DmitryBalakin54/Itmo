#!/bin/bash
grep -h -v '^\s*#' /etc/apt/sources.list /etc/apt/sources.list.d/*.list 2>/dev/null | awk '{print $2}' | sort -u

