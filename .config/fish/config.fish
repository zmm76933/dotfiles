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
if type -q eza
    alias ls "eza -g --icons"
    alias la "eza -a -g --icons"
    alias ll "eza -l -g --icons"
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
set -gx LC_COLLATE "ja_JP.UTF-8"
set -gx LC_CTYPE "ja_JP.UTF-8"
set -gx LC_MONETARY "ja_JP.UTF-8"
set -gx LC_NUMERIC "ja_JP.UTF-8"
set -gx EDITOR nvim
set -gx DOTPATH $HOME/.dotfiles
set -gx PATH ~/bin $PATH

# ASDF configuration code
if test -z $ASDF_DATA_DIR
    set _asdf_shims "$HOME/.asdf/shims"
else
    set _asdf_shims "$ASDF_DATA_DIR/shims"
end

# Do not use fish_add_path (added in Fish 3.2) because it
# potentially changes the order of items in PATH
if not contains $_asdf_shims $PATH
    set -gx --prepend PATH $_asdf_shims
end
set --erase _asdf_shims

# For direnv to work properly it needs to be hooked into the shell
eval (direnv hook fish)

# fzf
set -gx FZF_DEFAULT_OPTS "--height 40% --reverse --extended --ansi --multi --bind=ctrl-u:page-up --bind=ctrl-d:page-down --bind=ctrl-z:toggle-all"

# gpg
set -gx GPG_TTY (tty)

# SKK
set -gx SKKSERVER localhost

# Xapian for mu4e
set -gx XAPIAN_CJK_NGRAM japanese
