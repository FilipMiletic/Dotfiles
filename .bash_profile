#!/bin/bash

# Paths
export PATH="/usr/local/opt/llvm/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export GOPATH="$HOME/.go"
export PATH="$PATH:$GOPATH/bin"
###
export LSCOLORS="exfxcxdxbxegedabagacad"
force_color_prompt=yes
PROMPT_DIRTRIM=5

if [ -f /usr/local/share/bash-completion/bash_completion ]; then
	. /usr/local/share/bash-completion/bash_completion
fi

# Use 
# # Local disabling (current repository)
# git config bash.showDirtyState false
# if in big repo and slow prompt

export GIT_PS1_SHOWCOLORHINTS=1
export GIT_PS1_SHOWDIRTYSTATE=1

# Aliases
alias ls='ls -GH'
alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs"
alias gcc='gcc-8'
alias g++='g++-8'

COLOR_GIT_CLEAN='\[\033[1;34m\]'
COLOR_GIT_MODIFIED='\[\033[0;33m\]'
COLOR_GIT_STAGED='\[\033[0;35m\]'
COLOR_RESET='\[\033[0m\]'

parse_git_branch() {
     git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}

if [[ -z "$SSH_CLIENT" ]]; then
    # local connection, change prompt
	export PS1="\[\e[1;32m\]\w\[\e[m\] $(parse_git_branch)\\e[1;37m\]\$ \e[0m\]"
else
    # ssh connection, print hostname and os version
    echo "Welcome to $(scutil --get ComputerName) ($(sw_vers -productVersion))"
fi

function prompt() {
	export PS1="\[\e[1;32m\]\w\[\e[m\]\[\e[1;34m\]$(parse_git_branch) \[\e[1;37m\]\$ \[\e[0m\]"
}

PROMPT_COMMAND=prompt
