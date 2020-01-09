#!/bin/bash

if [ "$#" -lt 1 ]; then echo Usage: $0 output.txt; exit; fi

cat $1 | grep -i -v gdal |grep -i -v box |grep -i -v search |grep -i -v box |grep -i -v process |grep -i -v elapsed |grep -i -v origin |grep -i -v column | grep -i -v refine
