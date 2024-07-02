# Reset PATH
set -e PATH
eval (cat /etc/environment | sed -e 's/PATH/set -x PATH/' -e 's/[:=]/ /g' -e 's/"//g' -e 's/$/;/')

# Homebrew
set -e HOMEBREW_SHELLENV_PREFIX
eval (/home/linuxbrew/.linuxbrew/bin/brew shellenv)

alias update "brew update && brew upgrade && brew cleanup"
