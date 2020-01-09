[ -z "$PS1" ] && return # to not confuse scp and rsync

#echo now in bash profile

# On pfe, astrobeast, etc, switch to zsh, if not using it already
do_switch=$(uname -n | grep -i -E "pfe|mfe|lfe|astrobeast|spherescheetah|pipeline|centos|volar|decoder|oleg-linux")
shell=$(echo $SHELL | grep zsh)
if [ "$do_switch" != "" ] && [ "$shell" = "" ]; then 
    if [[ -f ~/.bash_login ]]; then source ~/.bash_login; fi
fi

#     # Interactive mode on do_switch
#     export HOME=/nobackupnfs2/oalexan1
#     cd
#     if [ -f ~/.bash_login ]; then source ~/.bash_login; fi
#if [ "$zula" != "" ]; then
    # Interactive mode on zulla
#    export HOME=/media/raid/oleg
#    cd
#    if [ -f ~/.bash_login ]; then source ~/.bash_login; fi
#else
# Non-interacive mode
if [ -f ~/.bashrc ]; then source ~/.bashrc; fi
#fi
