# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

unsetopt beep
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/Users/phil/.zshrc'
zstyle ':completion:*' list-colors ''
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
autoload -Uz compinit
compinit

# do not autoselect first completion entry
unsetopt menu_complete
unsetopt flowcontrol
# show completion menu on successive tab press
setopt auto_menu
setopt complete_in_word
setopt always_to_end

#case insensitive (all), partial-word and substring completion
if [[ "$CASE_SENSITIVE" = true ]]; then
	zstyle ':completion:*' matcher-list 'r:|=*' 'l:|=* r:|=*'
else
	if [[ "$HYPHEN_INSENSITIVE" = true ]]; then
		zstyle ':completion:*' matcher-list 'm:{a-zA-Z-_}={A-Za-z_-}' 'r:|=*' 'l:|=* r:|=*'
	else
		zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|=*' 'l:|=* r:|=*'
	fi
fi
unset CASE_SENSITIVE HYPHEN_INSENSITIVE


alias 'trans'='env TR_CURL_SSL_VERIFY=1 /Applications/Transmission.app/Contents/MacOS/Transmission'
alias 'ls'='ls -GFp'

source '/usr/local/bin/virtualenvwrapper.sh'

prompt='%n:%F{blue}%~/%f $ '
