#!/bin/bash

# Scale two images to have almost the same statistics

if [ "$#" -lt 2 ]; then echo Usage: $0 image1 image2; exit; fi

l=$1
r=$2

for f in $l $r; do
    
    stddev=3
    v=$(colormap.pl $f $stddev 2>/dev/null | grep -i "min and max")
    
    min=$(echo $v | perl -pi -e "s#\s+#\n#g" | head -n 5 | tail -n 1)
    max=$(echo $v | perl -pi -e "s#\s+#\n#g" | head -n 7 | tail -n 1)

    echo min=$min, max=$max

    f_scale=${f/.tif/_scale.tif}
    echo Go from $f to $f_scale
    
    image_calc -c "min(255, max(0, 255*(var_0-$min)/($max-$min)))" --output-nodata-value 0 \
	-d float32 $f -o $f_scale
done

for f in $l $r; do
    echo $f $(gdalinfo -stats $f | grep Maximum=)
done

for f in $l $r; do
    f_scale=${f/.tif/_scale.tif}
    echo $f_scale $(gdalinfo -stats $f_scale | grep Maximum=)
done

