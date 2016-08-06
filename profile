export PATH=/usr/local/bin:/usr/bin:/usr/local/sbin:/bin:/usr/sbin:/sbin:$PATH
export GOROOT="/usr/local/opt/go/libexec"
export GOPATH=$HOME/.go
export PATH=$PATH:$GOPATH/bin:$GOPATH/bin
export JAVA_HOME="$(/usr/libexec/java_home)"
export HOMEBREW_NO_ANALYTICS=1

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
