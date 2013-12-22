#!/bin/bash

# if [ "$#" -lt 1 ]; then echo Usage: $0 argName; exit; fi

source ~/.bashenv

user=$(whoami)
curr=$(uname -n)

for h in $user@$curr $user@pfe25 $Z $A $A2 $C3 $C6; do
    echo $h
    remote_copy.pl ~/bin/{kill_remote.sh,kill_by_name.sh,print_col.pl} $h
    ssh $h "export PATH=~/bin:$PATH; kill_by_name.sh autogen configure make gcc g++ fortran make sleep build launch_ perl python stereo time run test sed" 2>/dev/null
done

ssh $Z virsh destroy centos-64-5
ssh $Z virsh destroy centos-32-5
