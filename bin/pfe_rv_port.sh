#!/bin/bash

# If the following entry exists in .ssh/config on pfe:
# Host laptop
#  Hostname localhost
#  Port 9005
#  User oalexan1
#  CheckHostIP no

# then the commands below should be able to grant access from pfe to local machine

# Run on L1:
ssh pfx -N -f -R 9005:localhost:9005

# Run on local machine:
sl1 -N -f -R 9005:localhost:22

# Try this:
# On L1: ssh pfe22 -N -f -L 4012:localhost:22 -R 4007:localhost:22 -R 9005:localhost:9005

# For vnc:
# On L1:
# p=5982; q=5982; ssh pfx -N -f -L ${p}:localhost:${q}
# On local machine:
# p=5982; q=${p}; sl1 -N -f -L ${p}:localhost:${q}; vncviewer -PasswordFile=$HOME/.vnc/passwd localhost:${p} &
