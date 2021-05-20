if type -q exa
  alias ls "exa -g --icons"
  alias la "exa -a -g --icons"
  alias ll "exa -l -g --icons"
  alias lla "ll -a"
end

# homebrew
if type -q brew
  set -gx HOMEBREW_CASK_OPTS "--appdir=/Applications"
  alias update "brew update && brew upgrade && brew cleanup"
end

