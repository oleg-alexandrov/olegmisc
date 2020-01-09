#!/bin/bash

if [ "$#" -lt 1 ]; then echo Usage: $0 file.tif; exit; fi

~/bin/gsc.sh $1 10
