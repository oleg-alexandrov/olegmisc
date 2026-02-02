# Setup fzf
# ---------
if [[ ! "$PATH" == */home/oalexan1/.fzf/bin* ]]; then
  export PATH="/home/oalexan1/.fzf/bin:${PATH}"
fi

# Auto-completion (disabled due to compatibility issues)
# [[ $- == *i* ]] && source "/home/oalexan1/.fzf/shell/completion.bash" 2> /dev/null

# Key bindings (Ctrl-R for history search, etc.)
source "/home/oalexan1/.fzf/shell/key-bindings.bash"
