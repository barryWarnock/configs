autoload -Uz compinit
compinit
unsetopt beep
bindkey -e
# share history between tabs
setopt inc_append_history
setopt share_history
