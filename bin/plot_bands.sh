#!/bin/bash

if [ "$#" -lt 1 ]; then echo Usage: $0 img.tif; exit; fi

file=$1

band1=${file/.tif/_bandN.tif}
band2=${file/.tif/_bandE.tif}
band3=${file/.tif/_bandD.tif}

echo $band1 $band2 $band3




