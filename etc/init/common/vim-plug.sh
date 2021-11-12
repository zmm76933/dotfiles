#!/bin/bash

# Stop script if errors occur
# trap 'echo Error: $0:$LINENO stopped; exit 1' ERR INT
# set -eu

# Load vital library that is most important and
# constructed with many minimal functions
# For more information, see etc/README.md
. "$DOTPATH"/etc/lib/vital.sh

if is_linux; then
    eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
fi

# exit with true if you have anyenv command
if ! has "nvim"; then
    log_fail "error: this script is only supported with nvim"
    exit 1
fi

sh -c 'curl -fLo $HOME/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'

source $DOTPATH/etc/init/assets/neovim/install.sh

log_pass "nvim: vim-plug installed successfully"