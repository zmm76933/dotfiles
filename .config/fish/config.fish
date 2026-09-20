set fish_greeting ""

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
alias claude-yolo "claude --dangerously-skip-permissions"
alias cz chezmoi
command -qv nvim && alias vi nvim

# configuration directory
set -gx XDG_CONFIG_HOME $HOME/.config

# languages
set -gx LANG "en_US.UTF-8"
set -gx LC_ALL "en_US.UTF-8"
set -gx LC_COLLATE "ja_JP.UTF-8"
set -gx LC_CTYPE "ja_JP.UTF-8"
set -gx LC_MONETARY "ja_JP.UTF-8"
set -gx LC_NUMERIC "ja_JP.UTF-8"

# editor
set -gx EDITOR nvim

# platform
set -l ROOT_PATH (dirname (status --current-filename))
switch (uname)
    case Darwin
        source $ROOT_PATH/config-macos.fish
    case Linux
        source $ROOT_PATH/config-linux.fish
        if [ "(uname -r | grep microsoft)" != '' ]
          source $ROOT_PATH/config-windows.fish
        end
    case '*'
      # Do nothing
end

# path
set -gx DOTPATH $HOME/.dotfiles
set -gx PATH ~/bin $PATH
set -gx PATH ~/.local/bin $PATH
set -gx PATH ~/.docker/bin $PATH

# mise configuration code
if status is-interactive
    mise activate fish | source
else
    mise activate fish --shims | source
end

# fzf
set -gx FZF_DEFAULT_OPTS "--height 40% --reverse --extended --ansi --multi --bind=ctrl-u:page-up --bind=ctrl-d:page-down --bind=ctrl-z:toggle-all"

# task
set -gx GO_TASK_PROGNAME task

# zoxide
zoxide init fish  --cmd cd | source

# gpg
set -gx GPG_TTY (tty)

# SKK
set -gx SKKSERVER 127.0.0.1

# Xapian for mu4e
set -gx XAPIAN_CJK_NGRAM japanese
