#!/bin/bash

# Given the output of recursive grep, remove everything after column, and make unike.
perl -pi -e "s#:.*?\n#\n#g" | ~/bin/unique.pl

