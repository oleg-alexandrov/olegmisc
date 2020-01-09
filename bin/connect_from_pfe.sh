#!/bin/bash

# Always keep reverse forwarding sessions alive from pfe.

M=m.ndc.nasa.gov

index=$(uname -n |grep pfe | perl -pi -e "s#[^\d]##g")
if [ "$index" = "" ]; then exit; fi

~/bin/kill_by_name.sh try_connect.sh
sleep 2

for ((i=0; i < 2; i++)); do

    # ssh 
    if [ "$i" -eq 0 ]; then port=$index"0"$i; host="$port:localhost:22"; fi
        
    # vnc
    if [ "$i" -eq 1 ]; then
        vncport=$(ps ux | grep -i rfbport | grep -i -v grep |grep vnc | perl -pi -e "s#^.*?rfbport\s+(\d+).*?\$#\$1#g")
        if [ "$vncport" = "" ]; then continue; fi # no vnc started
        port=7000;  host="$port:localhost:$vncport"
    fi

    echo port and host $port $host
    
    prog=$(ps ux | grep $host | grep -v grep)
    if [ "$prog" == "" ]; then
        echo No connection, will start one
        echo "ssh -N -f $M -R $host"
        ssh -N -f $M -R $host
    else
        echo connection exists: $prog
    fi
    sleep 2

    # Verify if the reverse forwarding tunnel was opened properly.
    if [ "$i" -ne 0 ]; then continue; fi
    ans=$(ssh -o StrictHostKeyChecking=no -n -f $M bin/try_connect.sh $port)
    sshpid=$(ps ux | grep $host | grep -v grep | awk '{print $2}')
    if [ "$ans" != "0" ]; then
        echo "Tunnelling failed"
        # Tunneling failed, kill it, it will be restarted by cron soon.
        if [ "$sshpid" != "" ]; then kill -9 $sshpid; fi
    else
        echo "Tunneling succeded"
    fi
done
