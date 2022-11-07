# Reset PATH
set -e PATH
eval (/usr/libexec/path_helper -c | sed -e 's/setenv/set -x/' -e 's/:/ /g' -e 's/"//g')

# Homebrew
set -e HOMEBREW_SHELLENV_PREFIX
switch (uname -m)
case x86_64
  eval (/usr/local/bin/brew shellenv)
case arm64
  eval (/opt/homebrew/bin/brew shellenv)
end
set -gx HOMEBREW_CASK_OPTS "--appdir=/Applications"

alias update "brew update && brew upgrade && brew cleanup"

function brew
    set -xl PATH $PATH
    if type -q asdf; and contains $HOME/.asdf/shims $PATH
        set -e PATH[(contains -i $HOME/.asdf/shims $PATH)]
    end

    command brew $argv
end
