# First read the settings that are in common in bash and zsh
if [[ -f ~/.bash_login ]]; then source ~/.bash_login; fi
if [[ -f ~/.bashrc     ]]; then source ~/.bashrc;     fi

autoload -U compinit;   compinit
# autoload -U promptinit; promptinit

# # Disable completion with these as it is too slow
compdef -d svn make git vim

export HISTFILE=~/.zsh_history
export HISTSIZE=10000
export SAVEHIST=10000
setopt appendhistory
setopt incappendhistory
unsetopt extendedhistory
setopt histfindnodups
setopt histignorealldups
setopt histreduceblanks
unsetopt histignorespace # save to history commands that start with space
setopt nohistbeep
setopt histsavenodups
setopt sharehistory
setopt nohup            # don't kill jobs when exiting

setopt +o NOMATCH       # don't complain if there are no matches, just keep on going
unsetopt GLOB_SUBST     # do not expand "*" into the list of files
setopt AUTO_PUSHD       # make cd push the old directory onto the directory stack
setopt complete_in_word # complete when the cursor is in the middle of a word
setopt CSH_JUNKIE_LOOPS # for i in *; echo $i; end
setopt interactivecomments
setopt SH_WORD_SPLIT
setopt AUTO_CD # cd to given dir by just typing dir name
setopt PUSHD_IGNORE_DUPS

export WORDCHARS=""; # Make every non-alphanumeric be a word separator
#export WORDCHARS="*?_-.[]~=/&;!#$%^(){}<>"; # original

# To prevent funny editing
export ZLE_SPACE_SUFFIX_CHARS=$'&'
export ZLE_REMOVE_SUFFIX_CHARS=$'&'
bindkey " " self-insert

#eval `dircolors -b`

zstyle ':completion:*' menu select
zstyle ':completion:history-words:*:history-words' stop yes
zstyle ':completion:history-words:*:history-words' list no
zstyle ':completion:history-words:*' remove-all-dups yes
zstyle ':completion:history-words:*' menu yes

function expand-command-smartly () {

  # When some text is typed in the command line, expand it
  # intelligently to save typing.

  # Bind this function to a keystroke as follows:
  #zle -N expand-command-smartly
  #bindkey "^J" expand-command-smartly

  CURSOR_IN="$CURSOR"
  BUFFER_IN="$BUFFER"   
  CURSOR_OUT=$($HOME/projects/python/expand_cmdline.py "$BUFFER_IN" "$CURSOR_IN" "1")
  BUFFER_OUT=$($HOME/projects/python/expand_cmdline.py "$BUFFER_IN" "$CURSOR_IN" "2")
  BUFFER="$BUFFER_OUT"
  CURSOR=$(echo $CURSOR_OUT) # I guess this is needed to go from string to number
  #zle accept-line
}
zle -N expand-command-smartly

function proml
{

 # Show user@machine:/curdirr in the title bar
 if [[ $TERM == "xterm" || $TERM == "rxvt" ]]; then
   print -Pn "\e]2;%n@%m:%~\a";
 fi;

  # Must put escape characters in {% and %} to avoid garbling long command lines.
 PS1="
$terminfo[bold]%{$fg[green]%}%n@$(hostname | perl -p -e 's#\..*?$##g')%{$fg[white]%}:%{$fg[blue]%}%~
%{$fg[blue]%}%{$fg[green]%}%{$fg[red]%}>%{$fg[yellow]%}>%{$fg[green]%}>%{$fg[white]%} ";
}

bindkey  "^A"                beginning-of-line
bindkey  "^B"                backward-word
bindkey  "^E"                end-of-line
bindkey  "^D"                delete-char
bindkey  "^F"                forward-word
bindkey  "^H"                backward-delete-word
#bindkey  "^O"                expand-or-complete-prefix
bindkey  '^?'                backward-delete-char
bindkey  "^[[3~"             delete-char
bindkey  "^[3;5~"            delete-char
bindkey  "^R"                history-incremental-search-backward
bindkey  "^J"                expand-command-smartly
bindkey  "^H"                describe-key-briefly
bindkey  "^P"                up-line-or-history
#bindkey  "$terminfo[khome]" beginning-of-line
#bindkey  "$terminfo[kend]"  end-of-line
bindkey '^[[H'               beginning-of-line # mac
bindkey '^[[F'               end-of-line       # mac
bindkey  "^["                backward-delete-word
bindkey  "\M-d"              delete-word
bindkey  "^Z"                undo
bindkey  "^[d"               delete-word
bindkey  "^[[3;5~"           kill-line
bindkey  "^K"                kill-line
bindkey  "^[f"               forward-word
bindkey  "^[b"               backward-word
bindkey "\M-^?"              backward-delete-word
#bindkey " "                  magic-space
#bindkey '\e[15~' _history-complete-older #F5
#bindkey '\e[28~' _history-complete-newer #Shift-F5
bindkey "\M- "               _history-complete-older # completion from history
bindkey "\M-/"               _history-complete-older # completion from history

# function reread_aliases {
#   if [ -f ~/.unaliases    ]; then source ~/.unaliases;    fi;
#   if [ -f ~/.bash_aliases ]; then source ~/.bash_aliases; fi;
# }
# #add-zsh-hook preexec reread_aliases # older versions of zsh do not support this

# function save_curr_cmd {
#     # Copy the current command to byss, so that we can
#     # execute it from other machines
#     #ssh -f byss "echo \"$1\" > ~/.lastCmd" 1>/dev/null 2>&1
# }
# #add-zsh-hook preexec save_curr_cmd

# Colors
autoload -U colors
for cFile in $HOME/.zsh_colors                 \
    $w/local/share/zsh/4.3.10/functions/colors \
    /usr/share/zsh/functions/Misc/colors; do
  if [[ -f $cFile ]]; then
	source $cFile > /dev/null 2>&1
	break
   fi
done

# edit command line in full screen editor in zsh
autoload -z edit-command-line
zle -N edit-command-line
bindkey "^X^E" edit-command-line

# Init the prompt
proml

# # Source the ROS settings
# ros=/opt/ros/kinetic/setup.zsh
# if [ -f "$ros" ] && [ $(uname -n) != "oleg-linux" ]; then
#     source $ros
# fi

# PATH="/home/oalexan1/perl5/bin${PATH:+:${PATH}}"; export PATH;
# PERL5LIB="/home/oalexan1/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
# PERL_LOCAL_LIB_ROOT="/home/oalexan1/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
# PERL_MB_OPT="--install_base \"/home/oalexan1/perl5\""; export PERL_MB_OPT;
# PERL_MM_OPT="INSTALL_BASE=/home/oalexan1/perl5"; export PERL_MM_OPT;

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
function init_conda_zsh () {

__conda_setup="$('/home/oalexan1/miniforge3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/oalexan1/miniforge3/etc/profile.d/conda.sh" ]; then
        . "/home/oalexan1/miniforge3/etc/profile.d/conda.sh"
    else
        export PATH="/home/oalexan1/miniforge3/bin:$PATH"
    fi
fi
unset __conda_setup

if [ -f "/home/oalexan1/miniforge3/etc/profile.d/mamba.sh" ]; then
    . "/home/oalexan1/miniforge3/etc/profile.d/mamba.sh"
fi
}
# <<< conda initialize <<<


# fnm
FNM_PATH="/home/oalexan1/.local/share/fnm"
if [ -d "$FNM_PATH" ]; then
  export PATH="/home/oalexan1/.local/share/fnm:$PATH"
  eval "`fnm env`"
fi

