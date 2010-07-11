# First read the settings that are in common in bash and zsh
if [[ -f ~/.bash_login ]]; then source ~/.bash_login; fi
if [[ -f ~/.bashrc     ]]; then source ~/.bashrc;     fi

autoload -U compinit promptinit
compinit; promptinit;

export HISTFILE=~/.zsh_history
export z=~/.zsh_history
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

setopt CSH_NULL_GLOB    # don't complain if there are no matches
setopt NULL_GLOB        # don't complain if there are no matches
setopt AUTO_PUSHD       # make cd push the old directory onto the directory stack
setopt complete_in_word # complete when the cursor is in the middle of a word
setopt CSH_JUNKIE_LOOPS        # for i in *; echo $i; end
setopt interactivecomments
setopt SH_WORD_SPLIT
setopt AUTO_CD # cd to given dir by just typing dir name
setopt PUSHD_IGNORE_DUPS

export WORDCHARS=""; # Make every non-alphanumeric be a word separator
#export WORDCHARS="*?_-.[]~=/&;!#$%^(){}<>"; # original

eval `dircolors -b`

zstyle ':completion:*' menu select

autoload -U colors
localColors=$w/local/share/zsh/4.3.10/functions/colors; # local zsh installation
if [[ -f $localColors ]]; then source $localColors > /dev/null 2>&1; fi;

function expand-command-smartly () {

  # When some text is typed in the command line, expand it
  # intelligently to save typing.

  # Bind this function to a keystroke as follows: 
  #zle -N expand-command-smartly
  #bindkey "^J" expand-command-smartly

  BUFFER=$($HOME/bin/python/expand_cmdline.py "$BUFFER" $CURSOR)
  CURSOR=$(echo $BUFFER | sed -e 's/__sep__.*//') 
  BUFFER=$(echo $BUFFER | sed -e 's/^[0-9]*__sep__//') 
  
}
zle -N expand-command-smartly

function proml
{
    
 # Show user@machine:/curdirr in the title bar 
 if [[ $TERM == "xterm" || $TERM == "rxvt" ]]; then
   print -Pn "\e]2;%n@%m:%~\a"; 
 fi;

  if [[ $LOXIM_MODE = "" ]]; then
    LOX_TAG=""; 
    W_TAG="";
  else
    LOX_TAG="LOXIM_MODE=$LOXIM_MODE%{$fg[yellow]%}@";
    W_TAG="$W ";
  fi;

  # The command prompt. 
  # Must put escape characters in {% and %} to avoid garbling long command lines.
  PS1="
$terminfo[bold]%{$fg[green]%}%n@%m%{$fg[white]%}:%{$fg[blue]%}%~ 
%{$fg[blue]%}$LOX_TAG%{$fg[green]%}$W_TAG%{$fg[red]%}>%{$fg[yellow]%}>%{$fg[green]%}>%{$fg[white]%} ";
}

bindkey  "^A"                beginning-of-line
bindkey  "^B"                backward-word
bindkey  "^E"                end-of-line
bindkey  "^D"                delete-char
bindkey  "^F"                forward-word
bindkey  "^H"                backward-delete-word
bindkey  "^O"                expand-or-complete-prefix
bindkey  '^?'                backward-delete-char
bindkey  "^[[3~"             delete-char
bindkey  "^[3;5~"            delete-char
bindkey  "^R"                history-incremental-search-backward
bindkey  "^J"                expand-command-smartly
bindkey  "^H"                describe-key-briefly
bindkey  "^P"                up-line-or-history
bindkey  "$terminfo[khome]"  beginning-of-line
bindkey  "$terminfo[kend]"   end-of-line
bindkey  "^["                backward-delete-word
bindkey  "\M-d"              delete-word
bindkey  "^Z"                undo
bindkey  "^[d"               delete-word  
bindkey  "^[[3;5~"           kill-line    
bindkey  "^[f"               forward-word
bindkey  "^[b"               backward-word

# Init the prompt
proml;