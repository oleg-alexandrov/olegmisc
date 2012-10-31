#!/bin/sh

echo $*
/usr/bin/time -f "elapsed=%E mem=%M (kb) prog=$*" $*
