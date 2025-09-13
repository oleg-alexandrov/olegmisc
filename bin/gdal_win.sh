#!/bin/bash

# Take a GeoTiff as input. Print its four bounds in projected coordinates. Something
# that can be passed as the projwin option to ASP tools and to gdal_translate. 

if [ "$#" -lt 1 ]; then echo Usage: $0 argName; exit; fi

dem=$1
minmin=$2

a=$(gdalinfo $dem |grep -i "Upper Left" | perl -p -e "s#^.*?\((.*?),.*?\).*?\n#\$1#g")
b=$(gdalinfo $dem |grep -i "Upper Left" | perl -p -e "s#^.*?\(.*?,(.*?)\).*?\n#\$1#g")
c=$(gdalinfo $dem |grep -i "Lower Right" | perl -p -e "s#^.*?\((.*?),.*?\).*?\n#\$1#g")
d=$(gdalinfo $dem |grep -i "Lower Right" | perl -p -e "s#^.*?\(.*?,(.*?)\).*?\n#\$1#g")

# if minmin is empty, print as minx maxy maxx miny. Else
# print as minx miny maxx maxy
if [ "$minmin" == "" ]; then
  echo $a $b $c $d
else
  echo $a $d $c $b
fi
