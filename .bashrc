[ -z "$PS1" ] && return # to not confuse scp and rsync

# If we did not source ~/.bash_profile, do that first.
# It will then return here to continue.
if [ "$RAN_BASHPROFILE" = "" ]; then
    export RAN_BASHPROFILE=1
    if [ -f ~/.bash_profile ]; then
        source ~/.bash_profile
        return
    fi
fi

umask 022              # permissions set to -rw-r--r--

# Set these if the system says max number of threads exceeded
ulimit -s unlimited  > /dev/null 2>&1
ulimit -f unlimited  > /dev/null 2>&1
ulimit -v unlimited  > /dev/null 2>&1
ulimit -u unlimited  > /dev/null 2>&1

unset ignoreeof

set history=10000
set filec
set show-all-if-ambiguous on

## Pager macros
function mymore {
local MORE="/usr/bin/less -e"
#local MORE=more

if [ "$#" = 1 ]; then
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

function add_to_path () {
    # Prepend to PATH unless alrady first in the path
    if ! echo "$PATH" | grep -Eq "(^)$1($|:)" ; then
        export PATH="$1:$PATH"
        #echo New path $PATH
        #echo Already in the PATH=$PATH
    fi
}

function mb {
    cmd='PATH=~/projects/meshlab/distrib:$PATH meshlab $*'
    machine=$(uname -n | grep astrobeast)
    if [  "$machine" != "" ]; then
        # For astrobeast need to set the path to the right OpenGL libraries
        cmd='LD_LIBRARY_PATH=/usr/lib/x86_64-linux-gnu/mesa:$LD_LIBRARY_PATH '$cmd
    fi
    echo $cmd
    eval $cmd
}    

function num_fmt {
    echo $1 | perl -p -e "s#(\d+)#sprintf('%05d', \$1)#eg"
}    

function num_fmtd {
    # Format with given number of digits
    echo $1 | perl -p -e "s#(\d+)#sprintf('%0"$2"d', \$1)#eg"
}    

function rmp {
    pack=$1
    if [ "$pack" = "" ]; then
        echo empty input
    else
        perl -pi -e "s#^.*?$pack.*?\n##g" build_asp/done.txt
    fi
}

function gfr {
    grep -i "fail" report.txt | grep -v " failed" | perl -p -e "s#^.*?\[(.*?)\].*?\$#\$1#g"
}

function bmh {
  ~/freeflyer_build/native/devel/lib/sparse_mapping/build_map -info -output_map $1 | head -n 5
}

function rga {
    for f in $(gfr | print_col.pl 1); do
	echo $f;
	if [ ! -d "$f" ]; then
	    echo "Missing $f"
	    continue;
	fi
	cd $f
	rm -rfv gold; cp -rfv run gold
	cd ..
    done
}

function vt {
    # Clean up text like "file.cpp:123: other stuff" and keep only 
    # file.cpp:123, then call vscode on it.
    arg=$(echo "$*" | perl -p -e "s#^(.*?:\d+).*?\$#\$1#g")
    ~/projects/VSCode-linux-x64/bin/code-insiders --goto "$arg"
}

function tg {
    tail -n 1000 $1 | grep -i -v wait | tail -n 200
}

# Given an LRO NAC image and other stuff on each line, return names of
# mapprojected images in given dir with given convention. The first
# argument, $1, must be a directory.
function plm {
    dir=$1
    if [ ! -d "$dir" ]; then echo Not a directory $dir; return; fi
    perl -pi -e "s#^.*?(M.*?E).*?\n#${dir}/\$1.cal.echo.map.tr1.tif\n#g"
}

function cdls {
  if [ "$*" ]; then
     builtin cd "$*";
     ls -a --color=auto;
  else
      builtin cd; ls -a --color=auto;
  fi
  proml;
}

function ge {
    gdalinfo $1 | tail -n 8 | grep --colour=auto Mean=
}

function gis {
     gdalinfo $1 | grep -i --colour=auto size
}

function gim {
    gdalinfo -stats $1 | grep -i Maximum | grep -i --colour=auto mean
}

# Tail the latest file in current directory,
# or the n-th one if n is provided.
function tls {
    n=$1
    lines=$2
    if [ "$n" = "" ]; then
        n=1
    fi
    if [ "$lines" = "" ]; then
        lines=1000
    fi
    
    file=$(ls -altrdh * | tail -n $n | ~/bin/print_col.pl 0 | head -n 1)
    echo tail -n $lines $file
    tail -n $lines $file
}

function fn {
   find "$1" -name "$2"
}

function cvdel {
    rm -fv $1
    cvs delete $1
}

# after the W variable is updated, update the other settings
function setbuildenv
{
  export BASE=$HOME/$W/dev;
  export b=$HOME/$W/build;
  export bt=$HOME/$W/build/test;
  export bb=$HOME/$W/build/bin;
}

function gip {
    gdalinfo $1 | grep -i "pixel size";
}

function gipr {
    gdalinfo -proj4 $1 | grep -i "proj";
}

function a {

# Alias management

    if [ -f ~/.unaliases    ]; then source ~/.unaliases;    fi;
    if [ -f ~/.bash_aliases ]; then source ~/.bash_aliases; fi;

    if [ "$*" ]; then
        # Set the alias, and then display and save it
        alias "$*" > /dev/null # set the alias
        ans=$( alias "$*" | perl -p -e 's#(^|\n)(\w+=)#$1 . "a " . $2#eg' );
        if [ "$ans" != "" ]; then echo "$ans"; fi; # echo it

        ans2=$(echo "$*" | perl -p -e "s#[^=]##g")
        if [ "$ans2" != "" ]; then
            # We are actually setting an alias
            alias > ~/.bash_aliases;
            perl -pi -e "s#^([^\s]+=)#alias \$1#" ~/.bash_aliases
            ssh m.ndc.nasa.gov "echo alias '$*' | ~/bin/add_alias.pl" 2>/dev/null
        fi
    else
        alias  # Just list the aliases
    fi

}

# This calls the 'tail' function in several ways
function t {
        
    if [ "$#" -le 1 ]; then

        re='^[0-9]+$' # nice to know bash can use regexps
        if [[ "$1" =~ "$re" ]]; then
            # Was called as: cat file.txt | t 5
            tail -n $1
        else
            # Was called as: t file.txt
            tail -n 5 $1
        fi
        
    else
        # Was called as: t 20 file.txt
        tail -n $1 $2
    fi
}

function cll  {
    # Remove everything after column
    perl -p -e "s#:.*?\n#\n#g" | ~/bin/unique.pl | perl -p -e "s#\n# #g"
}

# Wipe distracting stuff from an ASP log file
function cl {
    perl -pi -e "s#.*?\]\s+:##g" $*
    perl -pi -e "s#^.*?\*\*.*?\$##g" $*
}

function ald {
    # Create an alias to cd to the current directory
    dir=$(echo $(pwd) | perl -p -e 's#'$HOME'#\$HOME#g')
    echo "a $1='cd $dir'"
    a $1="cd $dir"
}

function ag {
    # grep through all aliases for given pattern
    alias | grep -i --colour=auto "$*" | perl -p -e "s#^#a #g"
}

function eg {
    env | grep -i --colour=auto "$*"
}

function un {
  # Unalias an alias in all open and future sessions
  for u in $*; do
    unalias $u 2>/dev/null
    echo "unalias $u 2>/dev/null" >> ~/.unaliases
  done

  alias > ~/.bash_aliases
  perl -pi -e "s#^([^\s]+=)#alias \$1#g" ~/.bash_aliases
}

function pag {
    # grep through all history for given pattern
    ps aux | grep -i  "$*"
}

function pug {
    # grep through all history for given pattern
    ps ux | grep -i  "$*"
}

function hg {
    # grep through all history for given pattern
    history 1 | grep -i  "$*"
}

function gr {
  # recursive grep
  grep -r -i -n -E --colour=auto "$*" . --include="*.cc"  --include="*.cpp"  \
      --include="*.cxx" --include="*.h"  --include="*.hpp" --include="*.tcc" \
      --include="*.tex" --include="*.py" --include="*.in"  --include="*.c"   \
      --include="*.am" --include="*.m4" --include="*.xml"  --include="*.launch" \
      --include="*.java" 
}

function grr {
  # recursive grep every single file
  grep -r -i -n -E --colour=auto "$*" . 
}

function grf {
  # recursive grep every single file, then return the file names.
    grep -r -i -n -E --colour=auto "$*" . | perl -p -e "s#:.*?\n#\n#g" | ~/bin/unique.pl
}

# List all files matching the given pattern
function grf {
    val=$1
    gr $val | perl -p -e "s#:.*?\n#\n#g" | ~/bin/unique.pl
}

function fe {
 # Find files with given extension
  find . -name \*.$1;
}

function rh {
  perl -pi -e "s#$HOME#\\\$HOME/#g" .bash_aliases
}

function mcd {
    echo $(pwd) > $HOME/.currDir
}

function gcd {
    cd $(cat $HOME/.currDir)
}

function vp {
    echo $(readlink -f $1)
}

function st {
  grep -n $(ps ux |grep -i lt-reconstruct |grep -v grep | tail -n 1 | awk '{print $15}') imagesList.txt
}

function gse {
    grep -E -i "(start|end) job" $1 | diff_time.pl
}

function dm {
    # For the moon, convert degree per pixel to meters per pixel
    source ~/.bashenv
    ev $1\*$mmd
}

function s {

    # Detach and re-attach to screen number $n,
    # in the order given by 'screen -ls'
    n=$1
    ((n++)) # Skip the first status line
    id=$(screen -ls | head -n $n | tail -n 1 | awk '{print $1}')
    echo Will attach to screen $id
    screen -d -r $id
}

function tp () {
    remote_copy.pl $* oalexan1@pfx
}

function tb {
    remote_copy.pl $* $B
}

function tm () {
    remote_copy.pl $* $M
}

function f7 () {
    remote_copy.pl pipeline@centos7 $* 
}

function t7 () {
    remote_copy.pl $* pipeline@centos7
}

function tw () {
    remote_copy.pl $* oalexan1@wow
}


function ts () {
    remote_copy.pl $* oalexan1@spherescheetah
}

function fs () {
    remote_copy.pl oalexan1@spherescheetah $*
}

function tz {
    remote_copy.pl $* $Z
}

function fa {
    remote_copy.pl $A $*
}

function ta {
    remote_copy.pl $* $A
}

function ta2 {
    remote_copy.pl $* $A2
}

function tl1 {
    remote_copy.pl $* $L1
}

function tle {
    remote_copy.pl $* $(whoami)@lfe
}

function fd {
    remote_copy.pl $(whoami)@decoder $*
}

function td {
    remote_copy.pl $* $(whoami)@decoder
}

function tl {
    remote_copy.pl $* $(whoami)@laptop
}

function tl2 {
    remote_copy.pl $* $L2
}

function tl0 {
    remote_copy.pl $* oalexan1@laptop
}

function t3 {
    remote_copy.pl $* $C3
}

function t6 {
    remote_copy.pl $* $C6
}

function ta {
    remote_copy.pl $* oalexan1@astrobeast
}

function fa {
    remote_copy.pl oalexan1@astrobeast $*
}

function pco {
    # print largest number of matches
    cat $1 | grep haz |grep nav | sortbycol.pl 0
}

# read something like:
# myfile.cc:343 etc etc
# which is output by grep -r -i -N
# Write to disk the file and the line number in a format that we will read
# in xemacs with a simple keystroke
function v {
    fs="$HOME/.fileToOpen"
    val=$1
    val="$val:0" # in case the line is missing
    file=$(echo $val | perl -p -e 's#^(.*?):.*?$#$1#g')
    line=$(echo $val | perl -p -e 's#^.*?:(\d*).*?$#$1#g')
    if [ "$line" = "" ]; then line="0"; fi
    # Apply a fix changing /home6 to /home
    file=$(echo $file | perl -p -e "s#^\/home.*?/$(whoami)#$HOME#g")
    # make absolute
    file=$(readlink -f $file)
    if [ ! -f "$file" ]; then
        echo File $file does not exist
        return
    fi
    echo $file > $fs
    echo "line: $line" >> $fs
    cat $fs
}

function ovl {
  perl -pi -e "s#\"overwriteLayers\" \"false\"#\"overwriteLayers\" \"true\"#g" $1
  grep  -i --colour=auto overwriteLayers $1
}

function llt {
    ls -alh -rtd --color=auto $*
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

PS1="${TITLEBAR}\
\n$COLOR2${USER}@\h$COLOR3:$COLOR4\w\
\nbash $COLOR2>$COLOR3>$COLOR1>$COLOR_9 "
}

function cg {
    tail -n 10000000 $1 | grep -v Warn |grep -v col |grep -v range |grep -v actual |grep -v Tile |grep -v ROI

}

function sgm {
    min=$1; shift
    max=$1; shift
    stereo_gui --window-size 1600 1100 --colorbar --min $min --max $max $*
}

function sgi {
    min=$1; shift
    max=$1; shift
    stereo_gui --window-size 1600 1100 --colorbar --colormap-style inferno \
        --min $min --max $max $*
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
export FIGNORE=.o:~:.dvi:.aux:.toc:.bbl:.blg:.thm:.sty:.lof:.lot:.bib:.mexglx:.cmd:.ly2:.box

export GDBHISTFILE=$HOME/.gdb_history

export FVWM_USERDIR=$HOME/.fvwm # needed for fvwm

# More env variables
if [ -f ~/.bashenv ];   then source ~/.bashenv;   fi

if [ -f ~/.unaliases ]; then source ~/.unaliases; fi

# Aliases
if [ -f ~/.bash_aliases ]; then source ~/.bash_aliases; fi
