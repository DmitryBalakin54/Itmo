#!/bin/bash

apt policy | grep -E '^\s*[0-9]+|^\s*release'
