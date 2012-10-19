#!/bin/bash

# Kill all processes matching the given name

pids=""

progName=$(echo $0 | perl -pi -e "s#^.*\/##g")

for name in $*; do
    local_pids=$(ps ux | grep -i $name | grep -v grep | grep -v $progName | print_col.pl 2)
    pids="$pids $local_pids"
done

for pid in $(echo $pids | perl -pi -e "s#\s+#\n#g"); do
    echo "kill -9 $pid"
    kill -9 $pid
done