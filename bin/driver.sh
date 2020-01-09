#!/bin/bash

# Run the given program with given options in given directory on pfe.

if [ "$#" -lt 3 ]; then echo Usage: $0 out dir prog opts; exit; fi

echo "inputs are $*"
out=$1;  shift
dir=$1;  shift
prog=$1; shift
opts=$*

# Changing to the right directory
if [ "$dir" != "" ]; then cd $dir; fi

echo dir is $dir
echo prog is $prog

source ~/.bashenv
export PATH=$HOME/projects/StereoPipeline/src/asp/Tools:$HOME/projects/visionworkbench/src/vw/tools:$HOME/projects/base_system/bin:$HOME/projects/packages/bin:$HOME/bin:$PATH

. ~/bin/isis_setup.sh

# Ensure we compile for merope
# rsync -avz $M:projects/StereoPipeline/\* ~/projects/StereoPipeline2/; \cp -fv ~/projects/StereoPipeline/config.options ~/projects/StereoPipeline2/; cd ~/projects/StereoPipeline2/; ./autogen; ./configure --enable-app-point2mesh=no --enable-app-bundlevis=no; make -j 10

# Path for merope
v=$(stereo_fltr 2>&1 |grep -i "bin/sed" | perl -pi -e "s#\s##g")
if [ "$v" != "" ]; then
    export PATH=$HOME/projects/StereoPipeline2/src/asp/Tools:$PATH
fi

export ASP_PYTHON_MODULES_PATH=$HOME/projects/BinaryBuilder/StereoPipelinePythonModules/lib64/python2.6/site-packages:$HOME/projects/BinaryBuilder/StereoPipelinePythonModules/lib64/python2.6/site-packages/GDAL-1.10.0-py2.6-linux-x86_64.egg/osgeo:$HOME/projects/BinaryBuilder/StereoPipelinePythonModules/lib
export PYTHONPATH=$ASP_PYTHON_MODULES_PATH

# Need some care below. There could be a conflict between gdal_translate
# and sparse_disp
export LD_LIBRARY_PATH=$HOME/projects/base_system/lib/:$ASP_PYTHON_MODULES_PATH

export LD_LIBRARY_PATH=/nasa/pkgsrc/2014Q3/gcc49/lib:/nasa/pkgsrc/2014Q3/gcc49/lib64:/home/oalexan1/projects/zack_packages/local/lib:/home/oalexan1/projects/zack_packages/local/lib64:/u/oalexan1/projects/FreeFlyerMLP/build/external/opencv/src/opencv-build/lib

echo output goes to $out in $(pwd)
echo $prog $opts
umask 022

echo > $out

isPar=$(echo $prog |grep -i parallel_stereo)
if [ "$isPar" != "" ] && [ "$PBS_NODEFILE" != "" ]; then
    opts="$opts --nodes-list $PBS_NODEFILE"
    cat $PBS_NODEFILE >> $out
fi

echo $prog $opts >> $out
echo >> $out

$prog $opts >> $out 2>&1
