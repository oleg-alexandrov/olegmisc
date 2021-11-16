#!/bin/bash

# Copy a file to a different machine via port forwarding

if [ "$#" -lt 1 ]; then echo Usage: $0 fileName; exit; fi

source ~/.bashenv

#export M=lunokhod2

srcFile=$1
echo src is $srcFile

srcPort=2002 # 2002
dstPort=22 # as we do ssh
dstHost=localhost # localhost

# The dst file is the same as the src file but with any absolute path wiped.
dstFile=$(echo $srcFile | perl -p -e "s#$HOME\/##g" | perl -p -e "s#^\~\/##g")

echo dst is $dstFile
dstUser=oalexan1

# See if the destination machine is defined

val=$(echo $M | grep '@')
if [ "$val" = "" ]; then
    echo Must define the destination machine and assign it to variable M
    exit 1
fi

#echo Machine is $val

if [ 0 ]; then
# The ssh command we will use for port warding
sshCmd="ssh -l $dstUser -N -f $M -L $srcPort:${dstHost}:$dstPort"

# See if port forwarding is set
pid=$(ps ux | grep $srcPort:${dstHost}:$dstPort | ~/bin/print_col.pl 2 | head -n 1)
if [ "$pid" == "" ]; then
  # If not, set it
    echo $sshCmd
    eval $sshCmd 2>/dev/null
    status=$?
fi
fi

# Try to rsync
rsyncCmd="rsync -avz  -e 'ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -p $srcPort $dstUser@${dstHost}' $srcFile :$dstFile"
echo $rsyncCmd
eval $rsyncCmd 2>/dev/null
status=$?

echo Status is $status

# If the command succeeded, we are good
if [ "$status" -eq 0 ]; then
    echo success
    exit 0
fi

# Else kill the process and restart it
pid=$(ps ux | grep $srcPort:${dstHost}:$dstPort | ~/bin/print_col.pl 2 | head -n 1)
echo pid is $pid
if [ "$pid" != "" ]; then
    kill -9 $pid 2>/dev/null
fi

# Set up port forwarding again
echo $sshCmd
eval $sshCmd 2>/dev/null
status=$?

# Try second time
echo $rsyncCmd
eval $rsyncCmd 2>/dev/null
status=$?

echo Status is $status

# If the ssh command failed, there is nothing we can do
if [ "$status" -ne 0 ]; then
    echo Failed to ssh
fi

exit $status

