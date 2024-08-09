#!/bin/sh

# Kill all processes matching the given name

progName=$(echo $0 | perl -p -e "s#^.*\/##g")

pids=""
for name in $*; do
    ps ux | grep -i $name | grep -v $progName | grep -v grep | grep -v PID
    local_pids=$(ps ux | grep -i $name | grep -v $progName | grep -v grep | grep -v kill | grep -v PID | print_col.pl 2)
    pids="$pids $local_pids"
done

for pid in $(echo $pids | perl -p -e "s#\s+#\n#g"); do
    echo "kill -9 $pid"
    kill -9 $pid
done

