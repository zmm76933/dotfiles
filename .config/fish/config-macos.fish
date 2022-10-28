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

alias brew="env PATH=(string replace (pyenv root)/shims '' \"\$PATH\") brew"
alias update "brew update && brew upgrade && brew cleanup"
