# First read the settings that are in common in bash and zsh
if [[ -f ~/.bash_login ]]; then source ~/.bash_login; fi
if [[ -f ~/.bashrc     ]]; then source ~/.bashrc;     fi

autoload -U compinit;   compinit
autoload -U promptinit; promptinit

# Disable completion with these as it is too slow
compdef -d svn make git

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

setopt CSH_NULL_GLOB    # don't complain if there are no matches
setopt NULL_GLOB        # don't complain if there are no matches
unsetopt GLOB_SUBST       # expand "*" into the list of files
setopt AUTO_PUSHD       # make cd push the old directory onto the directory stack
setopt complete_in_word # complete when the cursor is in the middle of a word
setopt CSH_JUNKIE_LOOPS # for i in *; echo $i; end
setopt interactivecomments
setopt SH_WORD_SPLIT
setopt AUTO_CD # cd to given dir by just typing dir name
setopt PUSHD_IGNORE_DUPS

export WORDCHARS=""; # Make every non-alphanumeric be a word separator
#export WORDCHARS="*?_-.[]~=/&;!#$%^(){}<>"; # original

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
  
  BUFFER=$($HOME/bin/python/expand_cmdline.py "$BUFFER" $CURSOR)
  CURSOR=$(echo "$BUFFER" | sed -e 's/__sep__.*//') 
  BUFFER=$(echo "$BUFFER" | sed -e 's/^[0-9]*__sep__//') 
  
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
bindkey " "                  magic-space
#bindkey '\e[15~' _history-complete-older #F5
#bindkey '\e[28~' _history-complete-newer #Shift-F5
bindkey "\M- "               _history-complete-older # completion from history

function reread_aliases {
  if [ -f ~/.unaliases    ]; then source ~/.unaliases;    fi;
  if [ -f ~/.bash_aliases ]; then source ~/.bash_aliases; fi;
}
#add-zsh-hook preexec reread_aliases

# Colors
autoload -U colors
for cFile in $w/local/share/zsh/4.3.10/functions/colors \
             /usr/share/zsh/functions/Misc/colors       \
	     $HOME/.zsh_colors; do
  if [[ -f $cFile ]]; then 
  	source $cFile > /dev/null 2>&1 
	break
   fi
done

# Init the prompt
proml


