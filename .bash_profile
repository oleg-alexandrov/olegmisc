[ -z "$PS1" ] && return # to not confuse scp and rsync

# On pfe, astrobeast, etc, switch to zsh, if not using it already
do_switch=$(uname -n | grep -i -E "pfe|mfe|lfe|astrobeast|spherescheetah|pipeline|centos|volar|decoder|oleg-linux|oleg-VirtualBox|hivemind")
shell=$(echo $SHELL | grep zsh)
if [ "$do_switch" != "" ] && [ "$shell" = "" ]; then 
    if [[ -f ~/.bash_login ]]; then source ~/.bash_login; fi
fi

if [ -f ~/.bashrc ]; then source ~/.bashrc; fi
