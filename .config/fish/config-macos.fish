if type -q exa
  alias ls "exa -g --icons"
  alias la "exa -a -g --icons"
  alias ll "exa -l -g --icons"
  alias lla "ll -a"
end
if type -q brew
  alias update "brew update && brew upgrade && brew cleanup"
end
