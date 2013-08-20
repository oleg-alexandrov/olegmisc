#!/bin/sh

echo "$*"
echo "machine is $(uname -a) Job id is $PBS_JOBID num processors is $(cat /proc/cpuinfo  |grep processor | wc)"

echo start at $(date)
stime=$(date '+%s')

/usr/bin/time -f "elapsed=%E mem=%M (kb) prog=$*" $*

etime=$(date '+%s')
dt=$((etime - stime))
ds=$((dt % 60))
dm=$(((dt / 60) % 60))
dh=$((dt / 3600))
printf 'Elapsed: %d:%02d:%02d\n' $dh $dm $ds

echo finish at $(date)
echo " "
