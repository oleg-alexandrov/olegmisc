#!/bin/bash

#HOST=m.ndc.nasa.gov
HOST=lunokhod2

# Does not work on $HOST: -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no
prog=$(ps ux | grep localhost:22 | grep -v grep)
if [ "$prog" = "" ]; then
    ssh -l oalexan1 -N -f $HOST -L 2002:localhost:22
fi

prog=$(ps ux | grep localhost:5901 | grep -v grep)
if [ "$prog" = "" ]; then
    ssh -l oalexan1 -f -N $HOST -L 5001:localhost:5901
fi

prog=$(ps ux | grep localhost:5902 | grep -v grep)
if [ "$prog" = "" ]; then
    ssh -l oalexan1 -f -N $HOST -L 5002:localhost:5902
fi

# To subsequently do vnc on pfe
prog=$(ps ux | grep 7005:localhost:7000 | grep -v grep)
if [ "$prog" = "" ]; then
    ssh -l oalexan1 -f -N $HOST -L 7005:localhost:7000
fi
