# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=100
SAVEHIST=100
unsetopt appendhistory autocd beep
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/Users/phil/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

# Pure ZSH prompt

autoload -U promptinit && promptinit
prompt pure

# Alias
alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs -nw"
e() {
    emacsclient $1
}

# Set emacs as default editor
export EDITOR="/Applications/Emacs.app/Contents/MacOS/Emacs -nw"
