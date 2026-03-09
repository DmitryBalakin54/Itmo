#!/bin/bash

dumpe2fs /dev/sdb1 | grep -i journal
