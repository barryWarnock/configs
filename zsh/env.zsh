# Find and set branch name var if in git repository.
function git_branch_name()
{
    branch=$(git symbolic-ref HEAD 2> /dev/null | awk 'BEGIN{FS="/"} {print $NF}')
    if [[ $branch == "" ]];
    then
        :
    else
        echo ' ('$branch')'
    fi
}

# Enable substitution in the prompt.
setopt prompt_subst

prompt='%2/ $(git_branch_name) > '
#prompt
export PS1='%B%F{green}[%D{%I:%M}] %F{blue}%4d%F{cyan}$(git_branch_name)%f: %b'

#history variables
HISTFILE=$HOME/histfile
HISTSIZE=1000
SAVEHIST=10000

#editor
export EDITOR="vim"
export ALTERNATE_EDITOR=""
