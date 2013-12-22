#!/bin/bash

if [ "$#" -lt 1 ]; then echo Usage: $0 name; exit; fi

name=$1

gdal_translate -outsize 10% 10% run/run-DEM.tif run/run-DEM_sub10.tif
show_dems.pl $name"run" run/run-DEM_sub10.tif

gdal_translate -outsize 10% 10% gold/run-DEM.tif gold/run-DEM_sub10.tif
show_dems.pl $name"gold" gold/run-DEM_sub10.tif
