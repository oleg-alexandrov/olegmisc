# .bashrc

umask 022              # permissions set to -rw-r--r--
ulimit -f 2000000000   # max file size (200MB)
unset ignoreeof

set history=10000
set filec
set show-all-if-ambiguous on

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
    $MORE "$@"
fi
}

function cdls {
  if [ "$*" ]; then 
     builtin cd "$*"; ls -a --color;
  else
      builtin cd; ls -a --color;
  fi
  proml;
}

function rwd {

   # remember the working directory
   export WORKDIR=`pwd`;
 }

function cdw {
 cd $WORKDIR;
}

# after the W variable is updated, update the other settings
function setbuildenv
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

function a {

  # make an alias available to all other open shells right when it is defined
  if [ -f ~/.unaliases    ]; then source ~/.unaliases;    fi;
  if [ -f ~/.bash_aliases ]; then source ~/.bash_aliases; fi;

  if [ "$*" ]; then  
    alias "$*";
  else
    alias;  
  fi;

  alias > ~/.bash_aliases; 
  perl -pi -e "s#^([^\s]+=)#alias \$1#g" ~/.bash_aliases;
   
}

function ag {
    # grep through all aliases for given pattern
    alias | grep "$*"
}

function un {
  # Unalias an alias in all open and future sessions
  unalias "$*" 2>/dev/null;
  echo "unalias "$*" 2>/dev/null" >> ~/.unaliases;
  alias > ~/.bash_aliases; 
  perl -pi -e "s#^([^\s]+=)#alias \$1#g" ~/.bash_aliases;
}

function hg {
    # grep through all history for given pattern
    history 1 | grep "$*"
}

function svm {
    echo "svn commit -m \"$*\"";
    svn commit -m "$*";
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

 if [[ $LOXIM_MODE = "" ]]; then
   LOX_TAG="";
 else
   LOX_TAG="LOXIM_MODE=$LOXIM_MODE@$W ";
 fi;

PS1="${TITLEBAR}\
\n$COLOR2${USER}@\h$COLOR3:$COLOR4\w\
\n$LOX_TAG$COLOR2>$COLOR3>$COLOR1>$COLOR_9 "
}

# While this is an environment variable, it needs to be set here
# because it is used only interactively
if [ "$PS1" ]; then
  if [ $UID = 0 ]; then 
    export PS1="\h \! # "
  else 
    proml
 fi
fi

# Enviromental settings

# visualization and editing
export EDITOR=vim
export TEXEDIT='vim +%d %s'
export VISUAL=vim

# the two lines below cause trouble. 
#export MORE=less
#export LESS='XeiwmQPm--Less--?e (END):?s (%pb\%)..$PM--Less-- %f %bb/%s?e (END):?s (%pb\%)..'
export PAGER=less

# history
export HISTFILE=$HOME/.bash_history # Not to be confused with the csh history
export HISTSIZE=20000
export HISTCONTROL=ignoredups

# listing files
export LC_COLLATE=C # when using ls, put the dotfiles first.
export LS_COLORS="di=34;1:ln=36;1:ex=32;1:*~=31;1:*.zip=31;01:*.gz=31;01:*.bz2=31;01:*.tgz=31;1:*.gz=31;1:*.jpg=35;01:*.jpeg=35;01:*.gif=35;01:*.bmp=35;01:*.xpm=35;01:*.png=35;01:*.mov=35;01:*.mpg=35;01:*.mpeg=35;01:*.avi=35;01:*.xcf=35;01"
export FIGNORE=.o:.elc:~:.dvi:.aux:.toc:.log:.bbl:.blg:.thm:.sty:.lof:.lot:.bib:.mexglx:.cmd:.ly2:.box

# other
export MAILCHECK=-1800000 # don't notify of new mail

export GDBHISTFILE=$HOME/.gdb_history

# More env variables
if [ -f ~/.bashenv ]; then
        source ~/.bashenv
fi

# aliases
alias c=clear
alias cd=cdls
alias ce='crontab -e'
alias cp='\cp -iv'
alias cpu='cat /proc/cpuinfo'
alias cs=cvs_status.pl
alias ca='~/bin/loop_changes.sh'
alias csa='~/bin/loop_changes.sh'
alias css='cvs status | grep -i status | grep -v "Up-to-date"'
alias cvc='cvs commit -m '
alias cvd='cvs diff'
alias cvu='cvs update -P -d'
alias cx='chmod a+x'
alias d=cd
alias dh='du -h'
alias di=diff
alias ds='du . | sort -n'
alias fh='find . -name'
alias g='grep -i -E --colour=auto'
alias gr='g -r'
alias head='\head -n 100'
alias hp='nohup nice -19'
alias k9='kill -9'
alias ka='killall -v -9'
alias l='ls -a'
alias less=mymore
alias ll='ls -alh --color'
alias lls='ls -lSr  -a -h --color'
alias llt='ll -rt'
alias ls='ls -a --color'
alias lt='ll -rt'
alias m=mymore
alias make='time make'
alias matlab='matlab -nosplash'
alias mc='make clean'
alias md=mkdir
alias mk='time make'
alias ml='matlab -nosplash -nodesktop'
alias more=mymore
alias p=vim
alias pa='ps aux'
alias pico=vim
alias pl='perl -pi -e'
alias r='alias > ~/.bash_aliases; perl -pi -e "s#^([^\s]+=)#alias \$1#g" ~/.bash_aliases'
alias rr='source ~/.bash_login; source ~/.bashrc'
alias rf='\rm -fv'
alias rm='\rm -vi'
alias ssh='ssh -X'
alias svs='svn status'
alias svu='svn update'
alias tc='tar czfv'
alias tk=tkdiff
alias u='cd ..'
alias un=unalias
alias vv=/usr/bin/kcachegrind
alias which='alias=| /usr/bin/which --tty-only --read-alias --show-dot --show-tilde'
alias x=exit

if [ -f ~/.unaliases ]; then
        source ~/.unaliases
fi

# More aliases
if [ -f ~/.bash_aliases ]; then
        source ~/.bash_aliases
fi
