#!/bin/bash

# Paths
export PATH="/usr/local/opt/ruby/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"

###
# export LSCOLORS="exfxcxdxbxegedabagacad"
export LSCOLORS="exfxcxdxbxegedabagacad"
force_color_prompt=yes
PROMPT_DIRTRIM=5
export BASH_COMPLETION_COMPAT_DIR="/usr/local/etc/bash_completion.d"
[[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && . "/usr/local/etc/profile.d/bash_completion.sh"

# Use 
# # Local disabling (current repository)
# git config bash.showDirtyState false
# if in big repo and slow prompt

export GIT_PS1_SHOWCOLORHINTS=1
export GIT_PS1_SHOWDIRTYSTATE=1

vim_fzf() {
	vim "$(fzf)"
}
bind -x '"\C-t": vim_fzf'

# Aliases
alias ls='ls -GH'
alias python='python3'
alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs -nw"
alias ctags="`brew --prefix`/bin/ctags"

COLOR_GIT_CLEAN='\[\033[1;34m\]'
COLOR_GIT_MODIFIED='\[\033[0;33m\]'
COLOR_GIT_STAGED='\[\033[0;35m\]'
COLOR_RESET='\[\033[0m\]'

# parse_git_branch() {
#      git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
# }

function prompt() {
	export PS1="\[\e[1;32m\]\w\[\e[m\]\[\e[1;35m\] \[\e[1;37m\]\$ \[\e[0m\]"
}

export PROMPT_COMMAND=prompt
export PROMPT_COMMAND="$PROMPT_COMMAND;update_terminal_cwd;"

# Use `fd` as backend for FZF, and rg instead of grep
export FZF_DEFAULT_COMMAND='fd --type file --color=always'
export FZF_DEFAULT_OPTS="--ansi"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
