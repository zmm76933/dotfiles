set fish_greeting ""

# platform
switch (uname)
  case Darwin
    source (dirname (status --current-filename))/config-macos.fish
  case Linux
    source (dirname (status --current-filename))/config-linux.fish
    if [ "(uname -r | grep microsoft)" != '' ]
      source (dirname (status --current-filename))/config-windows.fish
    end
  case '*'
    # Do nothing
end

# theme
set -g theme_color_scheme terminal-dark
set -g fish_prompt_pwd_dir_length 1
set -g theme_display_user yes
set -g theme_hide_hostname no
set -g theme_hostname always

# aliases
if type -q exa
  alias ls "exa -g --icons"
  alias la "exa -a -g --icons"
  alias ll "exa -l -g --icons"
  alias lla "ll -a"
else
  alias ls "ls -p -G"
  alias la "ls -A"
  alias ll "ls -l"
  alias lla "ll -A"
end
alias g git
command -qv nvim && alias vi nvim
command -qv nvim && alias vim nvim

# environments
set -gx LANG "en_US.UTF-8"
set -gx LC_ALL "en_US.UTF-8"
set -gx LC_CTYPE "en_US.UTF-8"
set -gx EDITOR nvim
set -gx DOTPATH $HOME/.dotfiles
set -gx PATH ~/bin $PATH

# temporary setting
set -gx PATH $HOME/.asdf/shims $PATH
# asdf
source (brew --prefix asdf)/libexec/asdf.fish

# Go
set -gx GOPATH (go env GOPATH)
set -gx PATH $GOPATH/bin $PATH

# direnv
direnv hook fish | source

# fzf
set -gx FZF_DEFAULT_OPTS "--height 40% --reverse --extended --ansi --multi --bind=ctrl-u:page-up --bind=ctrl-d:page-down --bind=ctrl-z:toggle-all"

# gpg
set -gx GPG_TTY (tty)
