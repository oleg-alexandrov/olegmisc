source ~/.bash_login 
source ~/.bashrc

autoload -U compinit promptinit
compinit; promptinit;

export HISTFILE=~/.zsh_history
export HISTSIZE=10000
export SAVEHIST=10000
setopt appendhistory
setopt incappendhistory
setopt extendedhistory
setopt histfindnodups
setopt histignorealldups
setopt histreduceblanks
unsetopt histignorespace # save to history commands that start with space
setopt nohistbeep
setopt histsavenodups
setopt sharehistory  

setopt CSH_NULL_GLOB # don't complain if there are no matches
setopt NULL_GLOB     # don't complain if there are no matches
setopt AUTO_PUSHD    # Make cd push the old directory onto the directory stack

setopt PUSHD_IGNORE_DUPS

eval `dircolors -b`

zstyle ':completion:*' menu select

bindkey  "^A"                beginning-of-line
bindkey  "^E"                end-of-line
bindkey  "^D"                delete-char
bindkey  "^F"                forward-word
bindkey  "^H"                backward-delete-word
bindkey  '^?'                backward-delete-char
bindkey  "^[[3~"             delete-char
bindkey  "^[3;5~"            delete-char
bindkey  "^R"                history-incremental-search-backward
bindkey  "^H"                describe-key-briefly
bindkey  "$terminfo[khome]"  beginning-of-line
bindkey  "$terminfo[kend]"   end-of-line
bindkey  "^["                backward-delete-word
bindkey  "\M-d"              delete-word

autoload -U colors

function proml
{
    
 # Show user@machine:/curdirr in the title bar 
 if [[ $TERM == "xterm" || $TERM == "rxvt" ]]; then
   print -Pn "\e]2;%n@%m:%~\a"; 
 fi;

  if [[ $LOXIM_MODE = "" ]]; then
    LOX_TAG="";
  else
    LOX_TAG="LOXIM_MODE=$LOXIM_MODE@";
  fi;

  # The command prompt. 
  # Must put escape characters in {% and %} to avoid garbling long command lines.
  PS1="
$terminfo[bold]%{$fg[blue]%}%T %{$fg[yellow]%}%W %{$fg[green]%}%n@%m%{$fg[white]%}:%{$fg[blue]%}%~ 
%{$fg[blue]%}$LOX_TAG$W %{$fg[green]%}>%{$fg[yellow]%}>%{$fg[red]%}>%{$fg[white]%} "
}

proml;
