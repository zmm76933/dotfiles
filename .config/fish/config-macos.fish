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

# Reset PATH
set -e PATH
eval (/usr/libexec/path_helper -c | sed -e 's/setenv/set -x/' -e 's/:/ /g' -e 's/"//g')

# Homebrew
set -e HOMEBREW_SHELLENV_PREFIX
eval (/opt/homebrew/bin/brew shellenv)
set -gx HOMEBREW_CASK_OPTS "--appdir=/Applications"
alias update "brew update && brew upgrade && brew cleanup"

# fzf
set -g FZF_PREVIEW_FILE_CMD "bat --style=numbers --color=always --line-range :500"
set -g FZF_LEGACY_KEYBINDINGS 0
set -g FZF_ENABLE_OPEN_PREVIEW 1

function cpwd --description 'Copy the current directory to the clipboard, with $HOME as ~'
    printf '%s' (string replace -r "^$HOME" '~' -- $PWD) | pbcopy
end
