#!/bin/bash

systemctl list-unit-files --type=service | grep enabled
