#!/bin/bash

#if [ "$#" -lt 1 ]; then echo Usage: $0 argName; exit; fi

 export PATH=/home/oalexan1/projects/BinaryBuilder/build_asp/build/visionworkbench/visionworkbench-git/build_binarybuilder/src/vw/tools:/home/oalexan1/projects/BinaryBuilder/build_asp/install/bin:/home/oalexan1/projects/BinaryBuilder/build_asp/build/visionworkbench/visionworkbench-git/build_binarybuilder/src/vw/tools:/home/oalexan1/projects/BinaryBuilder/build_asp/install/bin:/home/oalexan1/projects/BinaryBuilder/build_asp/build/visionworkbench/visionworkbench-git/build_binarybuilder/src/vw/tools:/home/oalexan1/projects/BinaryBuilder/build_asp/install/bin:/home/pipeline/miniconda2/bin:/opt/rh/devtoolset-3/root/usr/bin:/Users/oalexan1/usr/local/bin:/home/smcmich1/programs/gcc_4_9_3_install/bin:/home/smcmich1/programs/latexmk/bin:/byss/smcmich1/programs/tkdiff-unix/:/Users/smcmich1/Library/Python/2.7/bin/:/home/pipeline/projects/gcc-4.9.3-install/bin:/home/oalexan1/projects/zack_packages/local/bin/:/home/pipeline/projects/packages/bin/:/Users/smcmich1/usr/local/bin:/home/oalexan1/.local/bin:/Users/oalexan1/.local/bin:/home/smcmich1/anaconda2/bin:/usr/local/bin:/usr/bin:/nasa/python/2.7.3/bin/:/nasa/sles11/git/1.7.7.4/bin/:/nasa/pkgsrc/2014Q3/gcc49/bin/:/nasa/svn/1.6.21/bin:/home/oalexan1/projects/isis/bin:/home/oalexan1/projects/BinaryBuilder/build_asp/install/bin:/home/oalexan1/projects/BinaryBuilder/latest/bin:/home/oalexan1/cmake_build/bin:/home/oalexan1/projects/packages/bin:/home/oalexan1/projects/zack_packages/local/bin:/home/oalexan1/projects/freeflyer/build/bin:/home/oalexan1/projects/StereoPipeline/src/asp/Tools:/home/oalexan1/projects/visionworkbench/src/vw/tools:/home/oalexan1/projects/packages/bin:/home/oalexan1/bin:/usr/lib64/qt-3.3/bin:/usr/local/bin:/bin:/usr/bin:/home/oalexan1/bin:/usr/local/sbin:/usr/sbin:/sbin:/usr/bin:/usr/local/bin:.

# PFE
mkdir -p ~/projects/data/.conda
ln -s ~/projects/data/.conda ~/

wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh
cx Miniconda3-latest-Linux-x86_64.sh
./Miniconda3-latest-Linux-x86_64.sh

# Install in /home6/oalexan1/projects/data/miniconda3
# This modifies .bashrc with the following text:

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home6/oalexan1/projects/data/miniconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home6/oalexan1/projects/data/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/home6/oalexan1/projects/data/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home6/oalexan1/projects/data/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

# Get gcc on pfe
conda install gxx_linux-64
export LD_LIBRARY_PATH=/home6/oalexan1/projects/data/miniconda3/x86_64-conda_cos6-linux-gnu/lib:$LD_LIBRARY_PATH
export PATH=/home6/oalexan1/projects/data/miniconda3/x86_64-conda_cos6-linux-gnu/bin:$PATH

#Create a new conda environment to install ISIS3 in
conda create -n isis3 python=3.6

#Activate the environment
#Depending on your version of Anaconda use one of the following:

#Anaconda 3.4 and up:
conda activate isis3

#Prior to Anaconda 3.4:
#source activate isis3

#Add the following channels to the environment
conda config --env --add channels conda-forge
conda config --env --add channels usgs-astrogeology

#Verify you have the correct channels:
conda config --show channels

#You should see:

channels:
    - usgs-astrogeology
    - conda-forge
    - defaults

#The order is important.  If conda-forge is before usgs-astrogeology, you will need to run:

conda config --env --add channels usgs-astrogeology

#Building
mkdir ~/projects/data
cd ~/projects/data
git clone git@github.com:USGS-Astrogeology/ISIS3.git
cd ISIS3
git submodule update --init --recursive

# conda env create -n isis3 -f environment_gcc4.yml       

# SUSE/Centos
#conda env update --file  environment_gcc4.yml

# Ubuntu
conda env create -n isis3 -f environment.yml

conda activate /home/oalexan1/.conda/envs/isis3

mkdir build install
cd build

mkdir -p $HOME/projects/isis3data
mkdir -p $HOME/projects/data/isis3data_test

#cmake -DCMAKE_INSTALL_PREFIX=$HOME/projects/ISIS3/install -Disis3Data=$HOME/projects/isis3data -DJP2KFLAG=OFF -Dpybindings=OFF -GNinja  ../isis

cmake -DCMAKE_INSTALL_PREFIX=$HOME/projects/ISIS3/install -Disis3Data=$HOME/projects/isis3data -Disis3TestData=$HOME/projects/isis3data_test -DJP2KFLAG=OFF -Dpybindings=OFF -GNinja  ../isis
#-DCMAKE_C_COMPILER=$HOME/projects/zack_packages/local/bin/gcc -DCMAKE_CXX_COMPILER=$HOME/projects/zack_packages/local/bin/g++  

export ISISROOT=$(pwd) # path to the build

# Dependencies are here:
#/home/oalexan1/.conda/envs/isis3/lib/libBullet3OpenCL_clew.so

#setisis .
#echo -e "alias setisis='. /usgs/cpkgs/isis3/isis3mgr_scripts/initIsisCmake.sh'" >> ~/.bashrc

# For libc for old systems
mkdir $HOME/projects/glibc; cd $HOME/projects/glibc 
wget http://ftp.gnu.org/gnu/glibc/glibc-2.14.tar.gz
tar zxvf glibc-2.14.tar.gz
cd glibc-2.14
mkdir build
cd build
../configure --prefix=$HOME/projects/glibc_install
make -j4
make install
# Also for old systems
conda install gxx_linux-64
#conda install gcc libgcc

#export LD_LIBRARY_PATH=/opt/rh/devtoolset-3/root/usr/lib64:/opt/rh/devtoolset-3/root/usr/lib:/home/pipeline/projects/gcc-4.9.3-install/lib:/home/pipeline/projects/gcc-4.9.3-install/lib64:/home/oalexan1/projects/zack_packages/local/lib:/home/oalexan1/projects/zack_packages/local/lib64

export LD_LIBRARY_PATH=$HOME/projects/glibc_install/lib:${LD_LIBRARY_PATH}

#export LD_LIBRARY_PATH=/opt/rh/devtoolset-3/root/usr/lib64:/opt/rh/devtoolset-3/root/usr/lib:/home/pipeline/projects/gcc-4.9.3-install/lib:/home/pipeline/projects/gcc-4.9.3-install/lib64:/home/oalexan1/projects/zack_packages/local/lib:/home/oalexan1/projects/zack_packages/local/lib64:$HOME/projects/glibc_install/lib:${LD_LIBRARY_PATH}

export LD_LIBRARY_PATH=$HOME/.conda/envs/isis3/x86_64-conda_cos6-linux-gnu/lib:$LD_LIBRARY_PATH
export PATH=$HOME/.conda/envs/isis3/x86_64-conda_cos6-linux-gnu/bin:$PATH

cd $ISISROOT
ninja install

conda config --add channels conda-forge
conda install csm

# not working
#conda install -c conda-forge laszip
#conda install -c auto liblas
#wget https://github.com/LASzip/LASzip/releases/download/3.4.1/laszip-src-3.4.1.tar.gz
#cmake .. -DCMAKE_INSTALL_PREFIX=$HOME/projects/BinaryBuilder/build_asp/install

Liblas
export LDFLAGS="$HOME/projects/BinaryBuilder/build_asp/install/lib/liblaszip.so  -std=c++11 -pthread"
cmake -DBoost_INCLUDE_DIR=/home6/oalexan1/projects/data/miniconda3/envs/isis3/include .. -DWITH_LASZIP=true -DLASZIP_INCLUDE_DIR=$HOME/projects/BinaryBuilder/build_asp/install/include -DWITH_GDAL=true -DGDAL_INCLUDE_DIR=$HOME/projects/BinaryBuilder/build_asp/install/include -DWITH_GEOTIFF=true -DGEOTIFF_INCLUDE_DIR=$HOME/projects/BinaryBuilder/build_asp/install/include -DGDAL_LIBRARY=$HOME/projects/BinaryBuilder/build_asp/install/lib/libgdal.so -DTIFF_LIBRARY_RELEASE:FILEPATH=/home6/oalexan1/projects/data/miniconda3/envs/isis3/lib/libtiff.so -DZLIB_LIBRARY_RELEASE:FILEPATH=/home6/oalexan1/projects/data/miniconda3/envs/isis3/lib/libz.so -DCMAKE_VERBOSE_MAKEFILE=ON -DLASzip_DIR:PATH=/home6/oalexan1/projects/data/miniconda3/envs/isis3/lib -DLASZIP_LIBRARY:FILEPATH=$HOME/projects/BinaryBuilder/build_asp/install/lib/liblaszip.so -DCMAKE_CXX_FLAGS="-std=c++11 -pthread"


 
for f in m4 libtool autoconf automake cmake bzip2 pbzip2 chrpath lapack zlib openssl curl png   jpeg tiff proj openjpeg2 libgeotiff   geos gdal   ilmbase openexr boost flann hdf5 eigen opencv parallel gsl xercesc cspice protobuf  superlu gmm osg3 qt qwt suitesparse tnt   jama laszip liblas geoid fgr   bullet embree nanoflann nn pcl armadillo isis gflags glog ceres   libnabo libpointmatcher imagemagick theia htdp; do g=$(grep $f environment_gcc4.yml); if [ "$g" = "" ]; then echo $f; fi; done

export PATH=/home6/oalexan1/projects/data/miniconda3/envs/isis3/bin:$PATH

m4
libtool
autoconf
automake
pbzip2
chrpath
lapack
#proj
openjpeg2
#libgeotiff 1.4.2
gdal
ilmbase
openexr
#boost
parallel
#xercesc xerces-c==3.1.4
osg3
laszip
liblas
geoid
fgr
#isis
gflags # check if exists!
glog   # check if exists!
ceres
libnabo
libpointmatcher
imagemagick
theia # I put a patch
htdp

# Also see the notes in build.py
# Read from: https://github.com/USGS-Astrogeology/ISIS3/wiki/Developing-ISIS3-with-cmake
cmake .. -DISIS_INSTALL_DIR=/home6/oalexan1/projects/ISIS3/install

    # Need to find ISIS install dir and conda dir for third party libraries
    # Read from: https://github.com/USGS-Astrogeology/ISIS3/wiki/Developing-ISIS3-with-cmake
    # /home/oalexan1/miniconda3/envs/isis3 on ubuntu
    # /home6/oalexan1/projects/data/miniconda3/envs/isis3 on pfe

# GDAL is flaky
# also liblas and libpointmatcher
# in conda need to deal with .la files
# Flann needs the empty hack like libpointmatcher!
# In ASP's CMakeLists.txt remove the temporary include_directories!

imagemagic uses libjpeg8

Cannot find
       libcholmod.so.3.0.6
        libGL.so.1
        libGLU.so.1
        libglut.so.3
        libjson-c.so.2
        libinput.so.10
        libxkbcommon.so.0
        libudev.so.1
        libQt5EglDeviceIntegration.so.5
        libEGL.so.1
        libjpeg.so.8
        libjbig.so.0
        libwebp.so.5
        libpangocairo-1.0.so.0
        libpango-1.0.so.0
        libspqr.so.2.0.2
        libcxsparse.so.3.1.4
        libpcre.so.3

# Deal with this!
perl -pi -e "s#/home/oalexan1/miniconda3/envs/isis3/lib/lib([\-\.\w]+).la#-l\$1#g" *la
perl -pi -e "s#(/[\w\/]*?lib)/lib([\-\.\w]+).la#-L\$1 -l\$2#g" ~/miniconda3/envs/isis3/lib/*la    

Need to document better how to install conda deps

conda env update environment.yml

# Need to get binutils 2.26.1 on Ubunt14 and then use gcc with the -B flag having the path
where this is installed.


# the new binuitls. Must have flex and bison
/home/pipeline/binutils-2.26.1
then Use -B$HOME/projects/packages in compliation
./configure --prefix=$HOME/projects/packages --enable-gold --enable-ld --enable-plugins

# On pfe
conda activate isis32 # very important

export LD_LIBRARY_PATH=/opt/rh/devtoolset-3/root/usr/lib64:/opt/rh/devtoolset-3/root/usr/lib:/home6/oalexan1/projects/data/gcc5/gcc-5.4.0/install/lib:/home/pipeline/projects/gcc-4.9.3-install/lib:/home/pipeline/projects/gcc-4.9.3-install/lib64:/home/oalexan1/projects/zack_packages/local/lib:/home/oalexan1/projects/zack_packages/local/lib64

# Put the D_GLIBCXX_USE_CXX11_ABI flag in CMakeLists.txt

~/miniconda3/envs/isis32/bin/cmake -DJP2KFLAG=OFF -Dpybindings=OFF -GNinja -DCMAKE_C_COMPILER=$HOME/projects/data/gcc5/gcc-5.4.0/install/bin/gcc5.4 -DCMAKE_CXX_COMPILER=$HOME/projects/data/gcc5/gcc-5.4.0/install/bin/g++5.4 -DCMAKE_CXX_FLAGS='-g -O3 -std=c++11 -D_GLIBCXX_USE_CXX11_ABI=0' -DCMAKE_VERBOSE_MAKEFILE=ON -DCMAKE_BUILD_TYPE=Release -DbuildTests=OFF -DBUILD_TESTS=OFF -DBUILD_TESTING=OFF ../isis


centos password: CentOSPipe

# It worked on pfe!
~/miniconda3/envs/isis33/bin/cmake -DJP2KFLAG=OFF -Dpybindings=OFF -GNinja -DCMAKE_CXX_FLAGS='-g -O3 -D_GLIBCXX_USE_CXX11_ABI=0' -DCMAKE_VERBOSE_MAKEFILE=ON -DCMAKE_BUILD_TYPE=Release -DbuildTests=OFF -DBUILD_TESTS=OFF -DBUILD_TESTING=OFF -DCMAKE_INSTALL_PREFIX=$(pwd)/../install ../isis -DCMAKE_C_COMPILER=$HOME/projects/data/gcc5/gcc-5.4.0/install/bin/gcc5.4 -DCMAKE_CXX_COMPILER=$HOME/projects/data/gcc5/gcc-5.4.0/install/bin/g++5.4; ninja install -v

# centos
conda activate isis3

~/miniconda3/envs/isis3/bin/cmake -DJP2KFLAG=OFF -Dpybindings=OFF -GNinja -DCMAKE_VERBOSE_MAKEFILE=ON -DCMAKE_BUILD_TYPE=Release -DbuildTests=OFF -DBUILD_TESTS=OFF -DBUILD_TESTING=OFF -DCMAKE_INSTALL_PREFIX=$(pwd)/../install -DCMAKE_C_FLAGS='-g -O3 -B/opt/rh/devtoolset-6/root' -DCMAKE_CXX_FLAGS='-g -O3 -D_GLIBCXX_USE_CXX11_ABI=0 -B/opt/rh/devtoolset-6/root' -DCMAKE_CXX_COMPILER=/opt/rh/devtoolset-6/root/bin/g++ -DCMAKE_C_COMPILER=/opt/rh/devtoolset-6/root/bin/gcc ../isis 

export ISISROOT=$(pwd)

./build.py --cxx=/opt/rh/devtoolset-6/root/bin/g++ --cc=/opt/rh/devtoolset-6/root/bin/gcc --gfortran=/opt/gfortran

./build.py --cxx=/home/pipeline/projects/gcc5/bin/g++ --cc=/home/pipeline/projects/gcc5/bin/gcc --gfortran=/opt/gfortran 
 
osgviewer fails on centos with gcc 6

# Build with gcc 5
sudo yum -y install gmp-devel mpfr-devel libmpc-devel glibc-devel glibc-devel.i686 zip unzip jar
wget https://ftp.gnu.org/gnu/gcc/gcc-5.4.0/gcc-5.4.0.tar.bz2
./configure --prefix=$HOME/projects/gcc5 --enable-gold=yes --enable-ld=yes --disable-multilib 

# Problem with ss_bundle_adjust_isis_ceres!
#also with ssISIS_alignAffEp_seedMode1_mapProj0_subPix1_frameCam

# This works
conda create --name sparse_disp -c conda-forge python=3.6 gdal
conda activate sparse_disp
conda install -c conda-forge scipy
conda install -c conda-forge pyfftw

camera_solve is broken
Need test with csm
Build the doc on centos7
