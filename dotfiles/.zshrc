source ~/.bash_login 

autoload -U compinit promptinit
compinit
promptinit

export HISTFILE=~/.zsh_history
export HISTSIZE=10000
export SAVEHIST=10000
setopt appendhistory
setopt incappendhistory
setopt extendedhistory
setopt histfindnodups
setopt histignorealldups
setopt histreduceblanks
setopt histignorespace
setopt nohistbeep
setopt histsavenodups
setopt sharehistory  

eval `dircolors -b`

zstyle ':completion:*' menu select

bindkey  "^A"      beginning-of-line
bindkey  "^E"      end-of-line
bindkey  "^D"      delete-char
bindkey  "^F"      forward-word
bindkey  "^H"      backward-delete-word
bindkey  '^?'      backward-delete-char
bindkey  "^[[3~"   delete-char
bindkey  "^[3;5~"  delete-char
bindkey  "^R"      history-incremental-search-backward

autoload -U colors

PS1="
$terminfo[bold]$fg[blue]%T $fg[yellow]%W $fg[green]%n@%m$fg[white]:$fg[blue]%~ 
$fg[blue]LOXIM_MODE=$LOXIM_MODE@$W $fg[green]>$fg[yellow]>$fg[red]>$fg[white] "

function mycd {
 if [ "$*" ]; then 
   \cd "$*"; ls -a --color;
 else
   \cd; ls -a --color;
 fi
}

source ~/.bash_aliases
