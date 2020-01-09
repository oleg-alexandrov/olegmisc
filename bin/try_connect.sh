#!/bin/bash

# Try to see if we can connect to a given machine on given port
if [ "$#" -lt 1 ]; then echo Usage: $0 port; exit; fi

port=$1
ans=$(ssh -o StrictHostKeyChecking=no -o BatchMode=yes -o ConnectTimeout=5 localhost -p $port echo 0 2>&1)
if [ "$ans" != "0" ]; then ans=1; fi
echo $ans
exit $ans
