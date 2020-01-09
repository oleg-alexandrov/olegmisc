#!/bin/bash

export PATH=$HOME/bin:$HOME/projects/base_system/bin:$PATH

cloud=$1
gdalinfo -stats $cloud | grep -i mean | grep -i max | tail -n 1
