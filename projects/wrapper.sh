#!/bin/bash

#if [ "$#" -lt 1 ]; then echo Usage: $0 argName; exit; fi

cd ~/projects/BinaryBuilder/
./auto_build/launch_master.sh local_mode
./upload.sh



