isbyss=$(uname -n | grep byss)
islunokhod=$(uname -n | grep lunokhod)

if [ "$isbyss" != "" ]; then
    echo "byss"
#   export ISISROOT=/byss/packages/isis_LRO_x86-64_linux_RHEL54/isis
    export ISISDATA=/byss/packages/isis3data

elif [ "$islunokhod" != "" ]; then
    echo "lunokhod"
#   export ISISROOT=$HOME/projects/isis
    export ISISDATA=/byss/packages/isis3data
else
    echo "Guessing Pleiades"
#   export ISISROOT=$HOME/projects/isis
    export ISISDATA=$HOME/projects/isis3data
fi

echo ISISROOT=$ISISROOT
echo ISISDATA=$ISISDATA

#$ISISROOT/scripts/isis3Startup.sh
#export PATH=$ISISROOT/bin:$PATH
