export GOPATH=$HOME/.go
export PATH=$PATH:$GOPATH/bin

# Homebrew collecting data disabled
export HOMEBREW_NO_ANALYTICS=1
export PATH="/usr/local/bin:$PATH"

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
