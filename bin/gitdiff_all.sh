#!/bin/bash

# This needs more work

a=fcd059c

rf tmp.txt; c=$a; for b in $(git lg | cat | pc 2 | grep -E "[0-9a-z]" | perl -pi -e "s#[^\w]# #g" | perl -pi -e "s#(\s*m|31m)##g" ); do q=$(git diff $b $c | cat | grep project_point_image); if [ "$q" != "" ]; then echo $b $c $q >> tmp.txt; fi; c=$b; done  



