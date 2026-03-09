#!/bin/bash

useradd -m -s /bin/bash -p $(openssl passwd -5 12345678) user
