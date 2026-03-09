#!/bin/bash

version=$(tail -n2 task16.log | head -n1)

apt-get install -y htop=$version
