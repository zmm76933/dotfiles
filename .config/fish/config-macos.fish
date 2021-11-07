# Homebrew
eval (/usr/local/bin/brew shellenv)
set -gx HOMEBREW_CASK_OPTS "--appdir=/Applications"

alias update "brew update && brew upgrade && brew cleanup"
