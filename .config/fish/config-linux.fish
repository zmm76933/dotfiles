if type -q eza
    alias ls "eza -g --icons=auto"
    alias la "eza -a -g --icons=auto"
    alias ll "eza -l -g --icons=auto"
    alias lla "ll -a"
end

if type -q bat
    alias bat "bat --theme \"Solarized (dark)\""
    alias less bat
end

alias fd fdfind

# Reset PATH
set -e PATH
eval (cat /etc/environment | sed -e 's/PATH/set -x PATH/' -e 's/[:=]/ /g' -e 's/"//g' -e 's/$/;/')

# Homebrew
set -e HOMEBREW_SHELLENV_PREFIX
eval (/home/linuxbrew/.linuxbrew/bin/brew shellenv)

alias update "brew update && brew upgrade && brew cleanup"
