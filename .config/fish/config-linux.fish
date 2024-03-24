# Reset PATH
set -e PATH
eval (cat /etc/environment | sed -e 's/PATH/set -x PATH/' -e 's/[:=]/ /g' -e 's/"//g' -e 's/$/;/')

# Homebrew
set -e HOMEBREW_SHELLENV_PREFIX
eval (/home/linuxbrew/.linuxbrew/bin/brew shellenv)

if type -q dnf
    alias update "sudo dnf -y update && dnf clean all"
else if type -q apt
    alias update "brew update && brew upgrade && brew cleanup && sudo apt update && sudo apt -y upgrade && sudo apt -y dist-upgrade && sudo apt -y autoremove && sudo apt -y autoclean"
end
