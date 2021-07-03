# homebrew
if type -q brew
  set -gx PATH /usr/local/opt/ncurses/bin $PATH
  set -gx HOMEBREW_CASK_OPTS "--appdir=/Applications"
  alias update "brew update && brew upgrade && brew cleanup"
end
