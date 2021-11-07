# Homebrew
eval (/usr/local/bin/brew shellenv)
set -gx HOMEBREW_CASK_OPTS "--appdir=/Applications"

alias brew="env PATH=(string replace (pyenv root)/shims '' \"\$PATH\") brew"
alias update "brew update && brew upgrade && brew cleanup"
