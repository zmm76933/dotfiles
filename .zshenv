# Add custom path to PATH
typeset -gx -U path
path=(
    ~/.fastlane/bin(N-/) \
    ~/.fastlane/bin/bundle/bin(N-/) \
    ~/.rbenv/bin(N-/) \
    ~/.pyenv/bin(N-/) \
    ~/.zplug/bin(N-/) \
    ~/.tmux/bin(N-/) \
    ~/bin(N-/) \
    /usr/local/sbin(N-/) \
    /usr/local/bin(N-/) \
    /usr/sbin(N-/) \
    /usr/bin(N-/) \
    /sbin(N-/) \
    /bin(N-/) \
    "$path[@]" \
    )

# NOTE: set fpath before compinit
typeset -gx -U fpath
fpath=( \
    ~/.zsh/Completion(N-/) \
    ~/.zsh/functions(N-/) \
    ~/.zsh/plugins/zsh-completions(N-/) \
    /usr/local/share/zsh/site-functions(N-/) \
    $fpath \
    )

# autoload
autoload -Uz run-help
autoload -Uz add-zsh-hook
autoload -Uz colors && colors
autoload -Uz compinit && compinit -u
autoload -Uz is-at-least

# HOSTNAME
export HOST=`hostname -s`

# LANGUAGE must be set by en_US
export LANGUAGE="en_US.UTF-8"
export LANG="${LANGUAGE}"
export LC_ALL="${LANGUAGE}"
export LC_CTYPE="${LANGUAGE}"

# Editor
export EDITOR=vim
export CVSEDITOR="${EDITOR}"
export SVN_EDITOR="${EDITOR}"
export GIT_EDITOR="${EDITOR}"

# Pager
export PAGER=less
# Less status line
export LESS='-R -f -X -i -P ?f%f:(stdin). ?lb%lb?L/%L.. [?eEOF:?pb%pb\%..]'
export LESSCHARSET='utf-8'

# LESS man page colors (makes Man pages more readable).
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[00;44;37m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

# ls command colors
export LSCOLORS=exfxcxdxbxegedabagacad
export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'

setopt no_global_rcs

# Settings for golang
export GOPATH="$HOME"
export GOBIN="$GOPATH/bin"

# Settings for rbenv
export RBENV_ROOT="$HOME/.rbenv"

# Settings for pyenv
export PYENV_ROOT="$HOME/.pyenv"

# fzf - command-line fuzzy finder (https://github.com/junegunn/fzf)
export FZF_DEFAULT_OPTS="--height 40% --reverse --extended --ansi --multi --bind=ctrl-u:page-up --bind=ctrl-d:page-down --bind=ctrl-z:toggle-all"

# Cask
export HOMEBREW_CASK_OPTS="--appdir=/Applications"

# keybind ^X^X
export ONELINER_FILE="$DOTPATH/doc/misc/commands.txt"

# declare the environment variables
export CORRECT_IGNORE='_*'
export CORRECT_IGNORE_FILE='.*'

export WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

# History file
export HISTFILE=~/.zsh_history
# History size in memory
export HISTSIZE=1000000
# The number of histsize
export SAVEHIST=1000000
# The size of asking history
export LISTMAX=50
# Do not add in root
if [[ $UID == 0 ]]; then
    unset HISTFILE
    export SAVEHIST=0
fi

# available $INTERACTIVE_FILTER
export INTERACTIVE_FILTER="fzf:peco:percol:gof:pick"

export DOTPATH="$HOME/.dotfiles"
