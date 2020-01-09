#!/bin/bash

dir=$HOME/bin/
cd $dir
svn update index.html
data=$(cat index.html | perl -pi -e "s#^.*?\:\s*([\d\.]+).*?\$#\$1#g")
host=$(echo $data| awk '{print $1}')
action=$(echo $data| awk '{print $2}')
port=$(echo $data| awk '{print $3}')

echo "port is   $port"
echo "host is   $host"
echo "action is $action"

if [ "$action" == "connect" ] && [ "$port" != "" ]; then 
        
    options="-f -N -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no"
    command="ssh $options -R $port:localhost:22 aoleg@$host -p 1979"
    exists=$(ps aux | grep "$command" | grep -v "grep")
    echo "command is $command"
    echo "exists is  $exists"

    if [ "$exists" == "" ]; then
        echo "Will connect"
        mailx oleg.alexandrov@gmail.com -s "see if mail arrives $port" < /dev/null  
        $command
    else
       echo "Will not connect"
       exit 0;
    fi

 else 
    echo "Will not connect"
    exit 0;
fi

