#!/bin/bash

export PATH="/usr/local/opt/llvm/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export GOPATH="$HOME/.go"
export PATH="$PATH:$GOPATH/bin"

force_color_prompt=yes
PROMPT_DIRTRIM=5
export LSCOLORS="exfxcxdxbxegedabagacad"

if [ -f /usr/local/share/bash-completion/bash_completion ]; then
	. /usr/local/share/bash-completion/bash_completion
fi

# Aliases
alias ls='ls -GH'
alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs"
alias gcc='gcc-8'
alias g++='g++-8'


COLOR_GIT_CLEAN='\[\033[1;34m\]'
COLOR_GIT_MODIFIED='\[\033[0;33m\]'
COLOR_GIT_STAGED='\[\033[0;35m\]'
COLOR_RESET='\[\033[0m\]'

function git_prompt() {
  if [ -e ".git" ]; then
    branch_name=$(git symbolic-ref -q HEAD)
    branch_name=${branch_name##refs/heads/}
    branch_name=${branch_name:-HEAD}

    echo -n ">> "

    if [[ $(git status 2> /dev/null | tail -n1) = *"nothing to commit"* ]]; then
      echo -n "$COLOR_GIT_CLEAN$branch_name$COLOR_RESET"
    elif [[ $(git status 2> /dev/null | head -n5) = *"Changes to be committed"* ]]; then
      echo -n "$COLOR_GIT_STAGED$branch_name$COLOR_RESET"
    else
      echo -n "$COLOR_GIT_MODIFIED$branch_name*$COLOR_RESET"
    fi

    echo -n " "
  fi
}


if [[ -z "$SSH_CLIENT" ]]; then
    # local connection, change prompt
   export PS1="\[\e[1;32m\]\w\[\e[m\] $(git_prompt)\\e[1;37m\]\$ \e[0m\]"
else
    # ssh connection, print hostname and os version
    echo "Welcome to $(scutil --get ComputerName) ($(sw_vers -productVersion))"
fi
 
function prompt() {
   export PS1="\[\e[1;32m\]\w\[\e[m\] $(git_prompt)\\e[1;37m\]\$ \e[0m\]"
}
PROMPT_COMMAND=prompt
