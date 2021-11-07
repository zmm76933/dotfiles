# Homebrew
eval (/usr/local/bin/brew shellenv)
set -gx HOMEBREW_CASK_OPTS "--appdir=/Applications"

function brew
    set -xl PATH $PATH # Protect global PATH by local PATH
    if type -q pyenv; and contains (pyenv root)/shims $PATH
        set -e PATH[(contains -i (pyenv root)/shims $PATH)]
    end

    command brew $argv
end

alias update "brew update && brew upgrade && brew cleanup"
