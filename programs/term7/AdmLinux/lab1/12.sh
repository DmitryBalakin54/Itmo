#!/bin/bash

useradd -m -s /bin/bash -p $(openssl passwd -5 87654321) u2
