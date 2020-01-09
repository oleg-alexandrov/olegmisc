#!/bin/bash

# Generate a list of images with captions

if [ "$#" -lt 2 ]; then
    echo Usage: $0 index.html dirs
    exit
fi

index=$1
echo Writing $index
shift
dirs=$*

# dir=$(dirname $index)
# mkdir -p $dir
currDir=$(pwd)
index=$currDir/$index

echo "<html>" > $index

echo "<h1>Intersection error, without (left) and with (right) jitter correction</h1>" >> $index


for dir in $dirs; do

    realDir=$(basename $(readlink -f $dir))

    satid=$(grep -i satid $dir/left.xml | head -n 1 | perl -pi -e "s#\<.*?\>##g")
    scandir=$(grep -i scandir $dir/left.xml | head -n 1 | perl -pi -e "s#\<.*?\>##g")
    tdi=$(grep -i tdi $dir/left.xml | head -n 1 | perl -pi -e "s#\s*\<.*?\>\s*##g")


    # empty lines for separation
    echo "<table>" >> $index
    echo "<tr><td> &nbsp;</td></tr>" >> $index
    echo "<tr><td> &nbsp;</td></tr>" >> $index

    echo "<tr><td>" >> $index
    echo SatID = $satid, ScanDir = $scandir, TDI = $tdi, Test case: $realDir >> $index
    echo "</td></tr>" >> $index

    f1=$dir/run_lr/run-IntersectionErr_CMAP

    if [ -d "$dir/run_lr_jiiter1000_yesba" ]; then
        mv $dir/run_lr_jiiter1000_yesba $dir/run_lr_jitter1000_yesba # bugfix
    fi

    f2=$dir/run_lr_jitter1000_yesba/run-IntersectionErr_CMAP

    if [ ! -f "${f1}_sub8.tif" ] || [ ! -f "${f2}_sub8.tif" ]; then
        g1=${f1/_CMAP/.tif}
        g2=${f2/_CMAP/.tif}

        if [ ! -f "${f1}.tif" ] || [ ! -f "${f2}.tif" ]; then
            ~/bin/colormap.pl $g1 $g2 -1
        fi

        stereo_gui --create-image-pyramids-only "${f1}.tif"
        stereo_gui --create-image-pyramids-only "${f2}.tif"
    fi

    # Pick the lowest resolution image for display
    sub=8
    if [ -f "${f1}_sub16.tif" ] && [ -f "${f2}_sub16.tif" ]; then
        sub=16
    fi
    if [ -f "${f1}_sub32.tif" ] && [ -f "${f2}_sub32.tif" ]; then
        sub=32
    fi

    if [ ! -f "${f1}.jpg" ]; then
        gdal_translate -of JPEG ${f1}_sub${sub}.tif ${f1}.jpg
    fi
    if [ ! -f "${f2}.jpg" ]; then
        gdal_translate -of JPEG ${f2}_sub${sub}.tif ${f2}.jpg
    fi

        echo "<tr><td><font color=red>No jitter corr</font></td>" >> $index
        echo "<td><font color=red>With jitter corr</font></td></tr>" >> $index

    echo "<tr><td><img src=\"$f1.jpg\"></img></td>" >> $index
    echo "    <td><img src=\"$f2.jpg\"></img></td></tr>" >> $index
    echo "</table>" >> $index

    remote_copy.pl ${f1}.jpg ${f2}.jpg +/byss/docroot/oalexan1/jitter oalexan1@byss

done

echo "</html>" >> $index

remote_copy.pl index.html +/byss/docroot/oalexan1/jitter oalexan1@byss

#index=${index/\/byss\/docroot\//}
#link=https://byss.arc.nasa.gov/$(whoami)/$index
#echo "$link"
# tac list.html > tmp.html
# echo "<p><a href=$link>$link</a>" >> tmp.html
# tac tmp.html > list.html
# echo "Appended to list.html"
