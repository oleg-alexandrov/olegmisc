#!/bin/bash

if [ "$#" -lt 5 ]; then echo Usage: $0 pwd tag seed win kernel; exit; fi

dir=$1
tag=$2
seed=$3 
win=$4
ker=$5

if [ "$dir" != "" ]; then cd $dir; fi

source ~/.bashenv

b=$(basename $(pwd))
p=$(echo $win | perl -pi -e "s#\s#_#g" | perl -pi -e "s#^_*##g" |  perl -pi -e "s#_*\$##g")

run=run$tag"_"$b"_"seed$seed"_"$p"_"ker$ker

outFile=output_"$run".txt
echo Will write to $outFile
exec &> $outFile 2>&1

export PATH=$HOME/projects/StereoPipeline/src/asp/Tools:$HOME/projects/base_system/bin:$HOME/projects/packages/bin:$HOME/bin:$PATH

# Path for merope
v=$(stereo_fltr 2>&1 |grep -i "bin/sed" | perl -pi -e "s#\s##g")
if [ "$v" != "" ]; then
    export PATH=$HOME/projects/StereoPipeline2/src/asp/Tools:$HOME/projects/base_system/bin:$HOME/projects/packages/bin:$HOME/bin:$PATH
fi
    
export ASP_PYTHON_MODULES_PATH=$HOME/projects/BinaryBuilder/StereoPipelinePythonModules/lib64/python2.6/site-packages:$HOME/projects/BinaryBuilder/StereoPipelinePythonModules/lib64/python2.6/site-packages/GDAL-1.10.0-py2.6-linux-x86_64.egg/osgeo:$HOME/projects/BinaryBuilder/StereoPipelinePythonModules/lib
export PYTHONPATH=$ASP_PYTHON_MODULES_PATH
export LD_LIBRARY_PATH=$ASP_PYTHON_MODULES_PATH

l=left_1mpp_crop_"$p".tif
r=right_1mpp_crop_"$p".tif
if [ "$p" = "0_0_100000_100000" ]; then
    ln -s left_1mpp_crop.tif $l
    ln -s right_1mpp_crop.tif $r
fi

if [ ! -f "$l" ]; then
    ~/bin/gdal_translate.pl -srcwin $win left_1mpp_crop.tif $l
fi
if [ ! -f "$r" ]; then
    ~/bin/gdal_translate.pl -srcwin $win right_1mpp_crop.tif $r
fi

cmd=stereo
nodes=""
if [ "$PBS_NODEFILE" != "" ]; then
    cmd=parallel_stereo
    echo Nodes are $(cat $PBS_NODEFILE)
    nodes="--threads-multiprocess 8 --threads-singleprocess 8 --nodes-list $PBS_NODEFILE"
fi

~/bin/time_run.sh $cmd $l $r left.xml right.xml $run/run krigged_dem_nsidc_ndv0_fill.tif -t dg --corr-seed-mode $seed --threads 32 -s stereo.default --subpixel-mode 1 --corr-timeout 800 --skip-image-normalization --disable-fill-holes --corr-max-levels 2  --subpixel-kernel $ker $ker --corr-kernel $ker $ker $nodes

point2dem -r Earth $run/run-PC.tif


