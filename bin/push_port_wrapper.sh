#!/bin/bash

if [ "$#" -lt 1 ]; then echo Usage: $0 remoteMachine; exit; fi

# A wraper around push_port.sh. For some reason it is is not possible
# to capture that command's "forwarding failed" message on stdin or
# stderr, hence writing it to a temporary file first then reading it.

# Also record this port on the remote machine.

machine=$1

# Read the last port used from a file, then increment it
portFile=/tmp/port_${machine}.txt
echo Reading the last-used port from $portFile. Will increment it.
port=$(cat $portFile) # if it does not exist, it will be 0, then incremented to 1000. 

ans="start"
outFile=/tmp/port_output.txt
while [ "$ans" != "" ]; do

    # Increment it. Make sure the port is between 5000 and 9999.
    ((port++)); 
    if [ "$port" -gt 9999 ]; then port=5000; fi
    if [ "$port" -lt 5000 ]; then port=5000; fi
    echo $port > $portFile # save it for next time
    
    echo Trying port $port
    push_port.sh $port $machine > ${outFile} 2>&1
    ans=$(grep "Warning:" ${outFile} | grep "forwarding failed")
    /bin/rm -f ${outFile}
    if [ "$ans" != "" ]; then
        echo $ans
    else
        # Modify the remote machine's .ssh/config to record this port
        ssh $machine bin/set_port.sh > /dev/null 2>&1
        echo Success
    fi
done
