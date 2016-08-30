# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=100
SAVEHIST=1000
unsetopt beep
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/Users/Filip/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

export PATH=/usr/local/bin:/usr/bin:/usr/local/sbin:/bin:/usr/sbin:/sbin:$PATH
export GOROOT="/usr/local/opt/go/libexec"
export GOPATH=$HOME/.go
export PATH=$PATH:$GOPATH/bin:$GOPATH/bin
export JAVA_HOME="$(/usr/libexec/java_home)"
export HOMEBREW_NO_ANALYTICS=1

# PURE prompt setup
fpath=("$HOME/.zfunctions" $fpath)
autoload -U promptinit && promptinit
prompt pure

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

# Some old habits
alias vim="nvim"

# Use Ctrl-T for opening FZF, and Ctrl-R for history
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
