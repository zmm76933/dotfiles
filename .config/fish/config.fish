set fish_greeting ""

set -gx TERM xterm-256color

# theme
set -g theme_color_scheme terminal-dark
set -g fish_prompt_pwd_dir_length 1
set -g theme_display_user yes
set -g theme_hide_hostname no
set -g theme_hostname always

# aliases
alias ls "ls -p -G"
alias la "ls -A"
alias ll "ls -l"
alias lla "ll -A"
alias g git
command -qv nvim && alias vi nvim
command -qv nvim && alias vim nvim

set -gx EDITOR nvim
set -gx PATH ~/bin $PATH
set -gx PATH /usr/local/sbin $PATH
set -gx DOTPATH "$HOME/.dotfiles"
set -gx HOMEBREW_CASK_OPTS "--appdir=/Applications"
set -gx FZF_DEFAULT_OPTS "--height 40% --reverse --extended --ansi --multi --bind=ctrl-u:page-up --bind=ctrl-d:page-down --bind=ctrl-z:toggle-all"

switch (uname)
  case Darwin
    source (dirname (status --current-filename))/config-macos.fish
  case Linux
    source (dirname (status --current-filename))/config-linux.fish
  case '*'
    # Do nothing
end
