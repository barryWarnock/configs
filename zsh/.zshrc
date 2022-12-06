[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

include () {
    [[ -f "$1" ]] && source "$1"
}

include $HOME/.zsh/alias.zsh
include $HOME/.zsh/env.zsh
include $HOME/.zsh/zsh_opts.zsh

bindkey -v
bindkey -M viins 'kj' vi-cmd-mode

bindkey '^P' up-history
bindkey '^N' down-history
bindkey '^r' history-incremental-search-backward
bindkey '^A' beginning-of-line
bindkey '^E' end-of-line

precmd() { RPROMPT="" }
function zle-line-init zle-keymap-select {
   VIM_PROMPT="%{$fg_bold[yellow]%} [% NORMAL]%  %{$reset_color%}"
   RPS1="${${KEYMAP/vicmd/$VIM_PROMPT}/(main|viins)/} $EPS1"
   zle reset-prompt
}

#pass through commands to git if they are valid gommands
function command_not_found_handler() {
    if git help $1 > /dev/null 2>&1; then
        git $@
    else
        echo "command not found: $1"
        return 127
    fi
}

zle -N zle-line-init
zle -N zle-keymap-select

export KEYTIMEOUT=10

if [ -f /home/barry.warnock/.ansible/env.sh ]; then
    . /home/barry.warnock/.ansible/env.sh
    # To disable ansible, comment out, but do not delete the following:
    activate_ansible
fi

#Set terminal window name to current dir
function chpwd() {
    print -Pn "\e]2;%~\a"
}

include $HOME/.local.zsh

export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"
# ~/bin takes precedence
export PATH="$HOME/bin:$PATH"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
