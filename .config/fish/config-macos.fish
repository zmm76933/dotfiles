# Reset PATH
set -e PATH
eval (/usr/libexec/path_helper -c | sed -e 's/setenv/set -x/' -e 's/:/ /g' -e 's/"//g')

# Homebrew
set -e HOMEBREW_SHELLENV_PREFIX
eval (/opt/homebrew/bin/brew shellenv)

set -gx HOMEBREW_CASK_OPTS "--appdir=/Applications"

alias update "brew update && brew upgrade && brew cleanup"
