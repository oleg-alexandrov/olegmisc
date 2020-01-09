isbyss=$(uname -n | grep byss)
islunokhod=$(uname -n | grep lunokhod)

if [ "$isbyss" != "" ]; then
    echo "byss"
    export ISISROOT=/byss/packages/isis_LRO_x86-64_linux_RHEL54/isis
    export ISIS3DATA=/byss/packages/isis3data

elif [ "$islunokhod" != "" ]; then
    echo "lunokhod"
    export ISISROOT=$HOME/projects/isis
    #export ISISROOT=$HOME/projects/base_system
    export ISIS3DATA=/byss/packages/isis3data
else
    echo "Guessing Pleiades"
    export ISISROOT=$HOME/projects/isis
    export ISIS3DATA=$HOME/projects/isis3data
fi

echo ISISROOT=$ISISROOT
echo ISIS3DATA=$ISIS3DATA

#$ISISROOT/scripts/isis3Startup.sh
export PATH=$ISISROOT/bin:$PATH
