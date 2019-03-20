[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

include () {
    [[ -f "$1" ]] && source "$1"
}

include $HOME/.zsh/alias.zsh
include $HOME/.zsh/env.zsh
include $HOME/.zsh/zsh_opts.zsh
include $HOME/.local.zsh

bindkey -v

bindkey '^P' up-history
bindkey '^N' down-history
bindkey '^r' history-incremental-search-backward

precmd() { RPROMPT="" }
function zle-line-init zle-keymap-select {
   VIM_PROMPT="%{$fg_bold[yellow]%} [% NORMAL]%  %{$reset_color%}"
   RPS1="${${KEYMAP/vicmd/$VIM_PROMPT}/(main|viins)/} $EPS1"
   zle reset-prompt
}

zle -N zle-line-init
zle -N zle-keymap-select

export KEYTIMEOUT=1

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
