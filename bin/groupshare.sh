#! /bin/bash

#Recursively set appropriate file and directory permissions on Pleiades, going up the filesystem as needed

#Usage is `groupshare.sh dir [gid]`
#e.g., `groupshare.sh conus_combined s2014`

indir="$1"

#Use default username
user=$(whoami)
#By default, use first group id listed
group=$(groups | awk '{print $1}')

if [ ! -z "$2" ] ; then
    group=$2
fi

#Standard read/execute
file_perm=640
dir_perm=750
#Standard read/write/execute
#file_perm=660
#dir_perm=770

#Set permissions upstream, stopping at user home directory
i=$(realpath $indir)
echo $i
while [[ $(basename "$i") != "$user" ]] 
do
    chmod -v $dir_perm $i ; chown -v :$group $i
    i=$(dirname $i)
done
#One more for top-level user directory - necessary to enable permission for subdirectories
chmod -v $dir_perm $i ; chown -v :$group $i
#Set permissions recursively downstream
#Apply to files individually
#find $indir -type d -exec chmod -v $dir_perm {} \; -exec chown -R :$group {} \; 
#find $indir -type f -exec chmod -v $file_perm {} \; -exec chown :$group {} \; 
#Apply to lists of files
find $indir -type d -exec chmod -v $dir_perm {} \+ -exec chown -v :$group {} \+ 
find $indir -type f -exec chmod -v $file_perm {} \+ -exec chown -v :$group {} \+ 

