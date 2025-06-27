# Find and set branch name var if in git repository.
function git_branch_name()
{
    branch=$(git symbolic-ref HEAD 2> /dev/null | awk 'BEGIN{FS="/"} {print $NF}')
    if [[ $branch == "" ]];
    then
        :
    else
        echo $branch
    fi
}

function git_path_or_full()
{
    if git rev-parse --is-inside-work-tree &> /dev/null
    then
        repo_name=$(basename `git rev-parse --show-toplevel`)
        sub_path=`git rev-parse --show-prefix`
        echo "%F{red}"$repo_name"%F{white}"@"%F{cyan}"`git_branch_name` "%F{blue}"$repo_name/$sub_path
    else
        echo "%F{blue}"`pwd`
    fi
    
}

# Enable substitution in the prompt.
setopt prompt_subst

#prompt
export PS1='%B%F{green}[%D{%I:%M}] $(git_path_or_full)%f: %b'

#history variables
HISTFILE=$HOME/histfile
HISTSIZE=1000
SAVEHIST=10000

#editor
export EDITOR="emacsclient -t"
export ALTERNATE_EDITOR="vim"
