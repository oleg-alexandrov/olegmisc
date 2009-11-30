# .bashrc

# Make the backspace behave
stty erase '^?'

# Bind the windows key for use in fvwm
xmodmap -e  "add mod3 = Super_L"

# Disable control s and control q, so that I can use them in vim.
#stty -a # see what is enabled
stty stop  undef # control s
stty start undef # control q

## Pager macros
function mymore {
local MORE=more

if [ $# = 1 ]; then
    case "$1" in
	*.tar.z|*.tar.gz|*.tgz)  gunzip -c "$@" | tar tf - | $MORE ;;
	*.tar.Z) zcat "$@" | tar tf - | $MORE ;;
	*.tar)   tar tf "$@" | $MORE ;;
	*.z|*.gz) gunzip -c "$@" | $MORE ;;
	*.tar.bz2) bzip2  -dc "$@" | tar tf - | $MORE ;;
	*.bz2) bzip2  -dc "$@" | $MORE ;;
	*.jpg) xv "$@" ;;
	*.Z)     zcat "$@" | $MORE ;;
	*.zip)   unzip -v "$@" | $MORE ;;
	*.man|*.[0-9]|*.[0-9][a-z]) nroff -man "$@" | $MORE ;; 
	*)       $MORE "$@" ;;
    esac
else
    more "$@"
fi
}

function mycd {
 if [ "$*" ]; then 
   \cd "$*"; ls -a --color;
 else
   \cd; ls -a --color;
 fi
}

function rwd {

   # remember the working directory
   export WORKDIR=`pwd`;
 }

function cdw {
 cd $WORKDIR;
}

function proml 
{
case $TERM in
        xterm*)
                local TITLEBAR='\[\033]0;\u@\h:\w\007\]'
                ;;
        *)
                local TITLEBAR=''
                ;;
esac

PS1="${TITLEBAR}\
\n$COLOR2${USER}@\h$COLOR3:$COLOR4\w\
\nLOXIM_MODE=$LOXIM_MODE@$W $COLOR2>$COLOR3>$COLOR1>$COLOR_9 "
}

# -<colour opc>--------------------------------
COLOR_0="\[\033[0;30m\]" # Black
COLOR0="\[\033[1;30m\]"  # Black (Light) / Dark Gray

COLOR_1="\[\033[0;31m\]" # Red (Dark)
COLOR1="\[\033[1;31m\]"  # Red

COLOR_2="\[\033[0;32m\]" # Green (Dark)
COLOR2="\[\033[1;32m\]"  # Green

COLOR_3="\[\033[0;33m\]" # Yellow (Dark) / Orange
COLOR3="\[\033[1;33m\]"  # Yellow

COLOR_4="\[\033[0;34m\]" # Blue (Dark)
COLOR4="\[\033[1;34m\]"  # Blue

COLOR_5="\[\033[0;35m\]" # Purple (Dark)
COLOR5="\[\033[1;35m\]"  # Purple
COLOR_6="\[\033[0;36m\]" # Cyan (Dark)
COLOR6="\[\033[1;36m\]"  # Cyan 

COLOR_7="\[\033[0;37m\]" # White (dark) / Light Gray
COLOR7="\[\033[1;37m\]"  # White

COLOR_8="\[\033[0;38m\]" # White
COLOR8="\[\033[1;38m\]"  # White Bold  

COLOR_9="\[\033[0;39m\]" # White
COLOR9="\[\033[1;39m\]"  # White Bold


# after the W variable is updated, update the other settings
function mysetvars
{
  export BASE=$HOME/$W/dev; 
  export b=$HOME/$W/build/;
  export bt=$HOME/$W/build/test;
  export bb=$HOME/$W/build/bin;
}

function vp
{
    # show the path to a file
    echo `pwd`/$1

}

# function ptv {

# # b=$1
# # a=`echo $b |sed -n s/\\..*$//p`
# #  echo $a

# # # a=`echo  $b |sed -n s/\\..*$//p`
# # # return;    
#  pdflatex $1.tex;
#  acroread -geometry 1200x990+0+0 $1.pdf;
# }

#enviromental variables
if [ -f ~/.bashenv ]; then
        . ~/.bashenv;
fi

# While this is an environment variable, it needs to be set here
# because it is used only interactively
if [ "$PS1" ]; then
  if [ $UID = 0 ]; then 
    export PS1="\h \! # "
  else 
    proml
 fi
fi

if [ -f ~/.bash_aliases ]; then
        . ~/.bash_aliases
fi

