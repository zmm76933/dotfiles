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

# exit with true if you have nvim command
if ! has "nvim"; then
    log_fail "error: this script is only supported with nvim"
    exit 1
fi

if [[ ! -e ~/.local/share/nvim/site/pack/packer/start/packer.nvim ]]; then
  git clone https://github.com/wbthomason/packer.nvim \
    ~/.local/share/nvim/site/pack/packer/start/packer.nvim
fi

nvim --headless +PackerInstall +qa
nvim --headless +TSUpdate +qa

source $DOTPATH/etc/init/assets/neovim/install.sh

log_pass "nvim: packer installed successfully"
