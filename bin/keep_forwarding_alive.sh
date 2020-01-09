#!/bin/bash

M=m.ndc.nasa.gov

# Does not work on $M: -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no
prog=$(ps ux | grep localhost:22 | grep -v grep)
if [ "$prog" = "" ]; then
    ssh -l oalexan1 -N -f $M -L 2002:localhost:22
fi

prog=$(ps ux | grep localhost:5901 | grep -v grep)
if [ "$prog" = "" ]; then
    ssh -l oalexan1 -f -N $M -L 5001:localhost:5901
fi

prog=$(ps ux | grep localhost:5902 | grep -v grep)
if [ "$prog" = "" ]; then
    ssh -l oalexan1 -f -N $M -L 5002:localhost:5902
fi

# To subsequently do vnc on pfe
prog=$(ps ux | grep 7005:localhost:7000 | grep -v grep)
if [ "$prog" = "" ]; then
    ssh -l oalexan1 -f -N $M -L 7005:localhost:7000
fi
